#lang racket

#|
from https://github.com/themetaschemer/net-pem/blob/6a2add18192a24118b13d0e652d808c270dd1890/net/pem/pem.rkt

MIT License

Copyright (c) 2020 anurag

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#

(require net/base64)

#|
  Parse PEM files (things like keys, certs etc.) for use with crypto

  The following is from RFC7468 (https://tools.ietf.org/html/rfc7468)

  textualmsg = preeb *WSP eol
               *eolWSP
               base64text
               posteb *WSP [eol]

  preeb      = "-----BEGIN " label "-----" ; unlike [RFC1421] (A)BNF,
                                           ; eol is not required (but
  posteb     = "-----END " label "-----"   ; see [RFC1421], Section 4.4)

  base64char = ALPHA / DIGIT / "+" / "/"

  base64pad  = "="

  base64line = 1*base64char *WSP eol

  base64finl = *base64char (base64pad *WSP eol base64pad /
                            *2base64pad) *WSP eol
                       ; ...AB= <EOL> = <EOL> is not good, but is valid

  base64text = *base64line base64finl
         ; we could also use <encbinbody> from RFC 1421, which requires
         ; 16 groups of 4 chars, which means exactly 64 chars per
         ; line, except the final line, but this is more accurate

  labelchar  = %x21-2C / %x2E-7E    ; any printable character,
                                    ; except hyphen-minus

  label      = [ labelchar *( ["-" / SP] labelchar ) ]       ; empty ok

  eol        = CRLF / CR / LF

  eolWSP     = WSP / CR / LF                        ; compare with LWSP

|#

;;-----------------------------------------------------------------
;; Terminals
;;-----------------------------------------------------------------

(define WSP "[ \t]")

(define eol "(\r\n|\r|\n)")

(define base64char "[[:alpha:][:digit:]+/]")

(define base64pad "=")

;; Printables except for hyphen.
(define labelchar "[a-zA-Z0-9!\"#$%^&*'()*+,./:;<=>?@\\[\\\\\\]^_`{|}~]")

(define *WSP (string-append WSP "*"))

(define eolWSP (string-append "(" WSP "|\r|\n)"))

(define *eolWSP (string-append eolWSP "*"))

(define eol? (string-append eol "?"))

;;-----------------------------------------------------------------
;; Helper Non-Terminals and Non-Terminals that don't matter
;;-----------------------------------------------------------------

(define base64line (string-append "(" base64char "+" *WSP eol ")"))

(define *base64line (string-append "(" base64line "*)"))

(define base64finl (string-append "(" base64char "*" base64pad "*" *WSP eol ")"))

;;-----------------------------------------------------------------
;; Non-Terminals that matter
;;-----------------------------------------------------------------

(define label (string-append "(" labelchar "([- ]?" labelchar ")*)"))

(define preeb (string-append "-----BEGIN " label "-----"))

(define base64text (string-append "(" *base64line base64finl ")"))

(define posteb (string-append "-----END " label "-----"))

;;-----------------------------------------------------------------
;; Main regexp
;;-----------------------------------------------------------------

(define pem-regexp (pregexp (string-append preeb *WSP eol *eolWSP base64text posteb *WSP eol?)))

(struct pem (label body))

(define (pem-string->pem pem-string)
  (let ([parsed (regexp-match pem-regexp pem-string)])
    (match pem-string
      [(regexp pem-regexp `(,full ,label ,_ ,_ ,_ ,body ,@_))
       (pem label (base64-decode (string->bytes/latin-1 body)))]
      [else (error 'pem-string->pem "Could not parse PEM string: ~a~%" pem-string)])))

(define (pem-string->pems pem-string)
  (let ([matches (regexp-match* pem-regexp pem-string #:match-select cdr)])
    (map (lambda (args)
            (pem (car args)
                (base64-decode (string->bytes/latin-1 (list-ref args 4)))))
        matches)))

(define (pem->pem-string pem)
  (string-append "-----BEGIN "
                 (pem-label pem)
                 "-----\r\n"
                 (bytes->string/latin-1 (base64-encode (pem-body pem)))
                 "-----END "
                 (pem-label pem)
                 "-----\r\n"))

(provide (struct-out pem)
         pem-string->pem
         pem-string->pems
         pem->pem-string)
