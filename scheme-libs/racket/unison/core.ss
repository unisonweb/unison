; This library implements various functions and macros that are used
; internally to the unison scheme libraries. This provides e.g. a
; measure of abstraction for the particular scheme platform. A useful
; feature of one implementation might need to be implemented on top of
; other features of another, and would go in this library.
;
; This library won't be directly imported by the generated unison
; code, so if some function is needed for those, it should be
; re-exported by (unison boot).
#!racket/base
(provide
  universal-compare
  chunked-string<?
  universal=?

  fx1-

  ; syntax->list
  (for-syntax raise-syntax-error)

  exception->string
  let-marks
  ref-mark

  chunked-string-foldMap-chunks

  freeze-bytevector!
  freeze-vector!
  freeze-subvector

  bytevector
  bytevector-append

  directory-contents
  current-microseconds

  decode-value
  describe-value

  bytevector->base32-string
  b32
  b32h

  bytevector->string/utf-8
  string->bytevector/utf-8)

(require
  racket/base
  (rename-in (only-in racket
                current-inexact-milliseconds
                directory-list
                string-copy!
                bytes
                bytes-append
                bytes?
                bytes=?
                bytes<?
                bytes->string/utf-8
                string->bytes/utf-8
                with-continuation-mark
                continuation-mark-set-first
                raise-syntax-error
                build-path
                path->string
                match
                for/fold)
          (string-copy! racket-string-copy!)
          (bytes-append bytevector-append)
          (string->bytes/utf-8 string->bytevector/utf-8)
          (bytes->string/utf-8 bytevector->string/utf-8)
          (bytes bytevector))
  (only-in rnrs/base-6 div mod div-and-mod)
  racket/exn
  (only-in racket/fixnum fl->fx fx- fxand fxlshift fxrshift fxior)
  racket/unsafe/ops
  unison/data
  unison/chunked-seq)

(define (fx1- n) (fx- n 1))

(define (decode-value x) '())

; 48 = #\0
; 87 = #\a - 10
(define (bytevector->base16-string bs)
  (define (b16 n) (integer->char (+ n (if (< n 10) 48 87))))

  (let* ([ilen (bytes-length bs)]
         [out (make-string (* 2 ilen))])
    (let rec ([i 0] [o 0])
      (cond
        [(>= i ilen) out]
        [else
          (let-values
            ([(c0 c1) (div-and-mod (bytes-ref bs i) 16)])
            (string-set! out o       (b16 c0))
            (string-set! out (+ o 1) (b16 c1))
            (rec (+ i 1) (+ o 2)))]))))

; code should convert 5-bit numbers to the corresponding character
(define (bytevector->base32-string code bs)
  (let* ([ilen (bytes-length bs)]
         [olen (* 8 (div (+ ilen 4) 5))]
         [out (make-string olen #\=)])

    (define (fill n k o)
      (if (>= k 0)
        (let ([m (fxand n 31)])
          (string-set! out (+ o k) (code m))
          (fill (fxrshift n 5) (- k 1) o))
        #f))

    (define (fixup i)
      (if (= i 0) (values 0 -1)
        (let ([bys (+ 1 (mod (- i 1) 5))])
          (let-values ([(d m) (div-and-mod (* 8 bys) 5)])
            (if (= m 0) (values m (- d 1))
              (values (- 5 m) d))))))

    (let rec ([acc 0] [i 0] [o 0])
      (cond
        [(>= i ilen)
         (let-values ([(n k) (fixup i)])
           (fill (fxlshift acc n) k o)
           out)]
        [(and (> i 0) (= 0 (mod i 5)))
         (fill acc 7 o)
         (rec (bytes-ref bs i) (+ i 1) (+ o 8))]
        [else
          (let ([sacc (fxlshift acc 8)]
                [by (bytes-ref bs i)])
            (rec (fxior sacc by) (+ i 1) o))]))))

; 65 = #\A
; 24 = #\2 - 26
(define (b32 n) (integer->char (+ n (if (< n 26) 65 24))))

; 48 = #\0
; 87 = #\a - 10
(define (b32h n) (integer->char (+ n (if (< n 10) 48 87))))


(define (describe-list op cl l)
  (let rec ([pre (string op)] [post (string op cl)] [cur l])
    (cond
      [(null? cur) post]
      [else
        (let* ([sx (describe-value (car cur))]
               [sxs (rec ", " (string cl) (cdr cur))])
          (string-append pre sx sxs))])))

(define (describe-list-sq l) (describe-list #\[ #\] l))
(define (describe-list-br l) (describe-list #\{ #\} l))

(define (describe-hash h)
  (substring (bytevector->base32-string b32h h) 0 8))

(define (describe-derived h i)
  (let ([th (describe-hash h)]
        [ti (if (= i 0) "" (string-append "." (number->string i)))])
    (string-append "#" th ti)))

(define (describe-ref r)
  (match r
    [(? symbol?) (symbol->string r)]
    [(unison-data r 0 (list name)) (string-append "##" name)]
    [(unison-data r 1 (list (unison-data s 0 (list bs ix))))
     (describe-derived bs ix)]
    [(unison-typelink-builtin name) (string-append "##" name)]
    [(unison-typelink-derived hash i) (describe-derived hash i)]))

(define (describe-bytes bs)
  (let* ([s (bytevector->base32-string b32h bs)]
         [l (string-length s)]
         [sfx (if (<= l 10) "" "...")])
    (string-append "32x" (substring s 0 10) sfx)))

(define (describe-value x)
  (match x
    [(unison-sum t fs)
     (let ([tt (number->string t)]
           [vs (describe-list-br fs)])
       (string-append "Sum " tt " " vs))]
    [(unison-data r t fs)
     (let ([tt (number->string t)]
           [rt (describe-ref r)]
           [vs (describe-list-br fs)])
       (string-append "Data " rt " " tt " " vs))]
    [(unison-pure v)
     (string-append "Pure " (describe-list-br (list v)))]
    [(unison-termlink-con r t)
     (let ([rt (describe-ref r)]
           [tt (number->string t)])
       (string-append "{Con " r " " t "}"))]
    [(unison-termlink-builtin name) (string-append "##" name)]
    [(unison-termlink-derived hash i) (describe-derived hash i)]
    [(unison-typelink-builtin nm)
     (string-append "##" nm)]
    [(unison-typelink-derived rf i)
     (let ([rt (describe-ref rf)]
           [it (if (= i 0) "" (number->string i))])
       (string-append "#" rt it))]
    [(unison-quote v)
     (string-append "{Value " (describe-value v) "}")]
    [(unison-code v)
     (string-append "Code (" (describe-value v) ")")]
    [(? chunked-list?)
     (describe-list-sq (vector->list (chunked-list->vector x)))]
    [(? chunked-string?)
      (format "\"~a\"" (chunked-string->string x))]
    [(? chunked-bytes?)
     (format
      "0xs~a"
      (chunked-string->string
       (for/fold
         ([acc empty-chunked-string])
         ([n (in-chunked-bytes x)])
         (chunked-string-append acc (string->chunked-string (number->string n 16))))))]
    [(? list?) (describe-list-sq x)]
    ; [(ilist? x) (describe-list-sq (list->mlist x))]
    [(? number?) (number->string x)]
    [(? string?) (string-append "\"" x "\"")]
    [(? bytes?) (describe-bytes x)]
    [else
      (format "~a" x)]))

(define (current-microseconds)
  (fl->fx (* 1000 (current-inexact-milliseconds))))

(define (directory-contents path-str)
  (define (extract path) (string->chunked-string (path->string path)))
  (map extract (directory-list (chunked-string->string path-str))))

(define (list-head l n)
  (let rec ([c l] [m n])
    (cond
      [(eqv? m 0) '()]
      [(null? c) '()]
      [else
        (let ([sub (rec (cdr c) (- m 1))])
          (cons (car c) sub))])))

; Simple macro to expand a syntactic sequence of comparisons into a
; short-circuiting nested comparison.
(define-syntax comparisons
  (syntax-rules ()
    [(comparisons c) c]
    [(comparisons c d ...)
     (let ([sc c])
       (case sc
         ['= (comparisons d ...)]
         [else sc]))]))

; universal-compares two lists of values lexicographically
(define (lexico-compare ls rs)
  (let rec ([cls ls] [crs rs])
    (cond
      [(and (null? cls) (null? crs)) '=]
      [else
        (comparisons
          (universal-compare (car cls) (car crs))
          (rec (cdr cls) (cdr crs)))])))

(define (cmp-num l r)
  (cond
    [(= l r) '=]
    [(< l r) '<]
    [else '>]))

(define (universal-compare l r)
  (cond
    [(equal? l r) '=]
    [(and (number? l) (number? r)) (if (< l r) '< '>)]
    [(and (chunked-list? l) (chunked-list? r)) (chunked-list-compare/recur l r universal-compare)]
    [(and (chunked-string? l) (chunked-string? r))
     (chunked-string-compare/recur l r (lambda (a b) (if (char<? a b) '< '>)))]
    [(and (chunked-bytes? l) (chunked-bytes? r))
     (chunked-bytes-compare/recur l r (lambda (a b) (if (< a b) '< '>)))]
    [(and (bytes? l) (bytes? r))
     (cond
       [(bytes=? l r) '=]
       [(bytes<? l r) '<]
       [else '>])]
    [(and (unison-data? l) (unison-data? r))
     (let ([fls (unison-data-fields l)] [frs (unison-data-fields r)])
       (comparisons
         (cmp-num (unison-data-tag l) (unison-data-tag r))
         (cmp-num (length fls) (length frs))
         (lexico-compare fls frs)))]
    [else
      (let ([dl (describe-value l)]
            [dr (describe-value r)])
        (raise
          (format
            "universal-compare: unimplemented\n~a\n\n~a"
            dl dr)))]))

(define (chunked-string<? l r) (chunked-string=?/recur l r char<?))

(define (universal=? l r)
  (define (pointwise ll lr)
    (let ([nl (null? ll)] [nr (null? lr)])
      (cond
        [(and nl nr) #t]
        [(or nl nr) #f]
        [else
          (and (universal=? (car ll) (car lr))
               (pointwise (cdr ll) (cdr lr)))])))
  (cond
    [(equal? l r) #t]
    [(and (unison-code? l) (unison-code? r))
     (universal=? (unison-code-rep l) (unison-code-rep r))]
    [(and (chunked-list? l) (chunked-list? r))
     (chunked-list=?/recur l r universal=?)]
    [(and (unison-data? l) (unison-data? r))
     (and
       (eqv? (unison-data-tag l) (unison-data-tag r))
       (pointwise (unison-data-fields l) (unison-data-fields r)))]
    [else #f]))

(define (exception->string e) (string->chunked-string (exn->string e)))

; (define (syntax->list stx)
;   (syntax-case stx ()
;     [() '()]
;     [(x . xs) (cons #'x (syntax->list #'xs))]))

(define (call-with-marks rs v f)
  (cond
    [(null? rs) (f)]
    [else
      (with-continuation-mark (car rs) v
        (call-with-marks (cdr rs) v f))]))

(define-syntax let-marks
  (syntax-rules ()
    [(let-marks ks bn e ...)
     (call-with-marks ks bn (lambda () e ...))]))

(define (ref-mark k) (continuation-mark-set-first #f k))

(define (chunked-string-foldMap-chunks s m f)
  (for/fold
      ([acc empty-chunked-string])
      ([c (in-chunked-string-chunks s)])
    (f acc (string->chunked-string (m c)))))

(define freeze-bytevector! unsafe-bytes->immutable-bytes!)

(define freeze-vector! unsafe-vector*->immutable-vector!)

(define (freeze-subvector src off len)
  (let ([dst (make-vector len)])
    (let next ([i (fx1- len)])
      (if (< i 0)
        (begin
          (freeze-vector! dst)
          (sum 1 dst))
        (begin
          (vector-set! dst i (vector-ref src (+ off i)))
          (next (fx1- i)))))))
