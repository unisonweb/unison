#lang racket/base
(require racket/exn
         racket/string
         racket/file
         rnrs/io/ports-6
         (only-in racket empty?)
         compatibility/mlist
         (only-in unison/boot data-case define-unison)
         unison/data
         unison/chunked-seq
         unison/core
         unison/tcp
         unison/pem
          unison/core
          unison/data
          unison/math
          unison/chunked-seq
          unison/chunked-bytes
          unison/bytes-nat
          unison/pattern
          unison/crypto
          unison/data
          unison/io
          unison/tls
          unison/tcp
          unison/gzip
          unison/zlib
          unison/concurrent
         )


(provide
 (prefix-out
  builtin-IO.
  (combine-out
    seekHandle.impl.v3
    getLine.impl.v1
    ))

;    getBuffering.impl.v3

;    handlePosition.impl.v3 ; hTell
    ; (port-position port)

    ; set-port-position!
;    isSeekable.impl.v3
    ; port-has-set-port-position!?
    ; https://docs.racket-lang.org/r6rs/r6rs-lib-std/r6rs-lib-Z-H-9.html#node_idx_658

;    getChar.impl.v1
;    ready.impl.v1

;    isFileOpen.impl.v3
;    isFileEOF.impl.v3

; SKIPPING:
;    setEcho.impl.v1
;    getEcho.impl.v1
;    setBuffering.impl.v3

   )

(define either-id (bytevector 6 15 103 128 65 126 44 164 169 154 106 164 187 86 33 156 155 89 79 64 71 158 119 151 142 79 121 206 247 92 41 13 151 250 243 205 13 193 134 218 198 145 193 96 55 87 92 215 34 52 161 162 226 22 169 43 228 184 86 77 149 58 66 125))
(define failure-ability-id (bytevector 216 146 217 169 19 75 109 57 228 133 238 216 198 69 74 234 144 236 6 38 105 162 165 108 46 84 142 76 232 51 233 44 127 14 173 141 116 116 188 249 139 240 83 187 68 90 188 52 255 88 237 69 19 14 135 123 195 98 242 101 194 85 63 61))

(define (Right value)
    (data (data 'Reference 1 (data 'Id 0 either-id 0)) 0 value))

; typeLink msg any
(define (Exception typeLink message payload)
    (let* ([x7 (data (data 'Reference 0 "Any") 0 payload)]
            [x8 (data (data 'Reference 1 (data 'Id 0 failure-ability-id 0)) 0 typeLink message x7)])
    (data (data 'Reference 1 (data 'Id 0 either-id 0)) 1 x8)))

(define-unison (seekHandle.impl.v3 handle mode amount)
    (data-case mode
        (0 ()
            (set-port-position! handle amount)
            (Right none))
        (1 ()
            (let ([current (port-position handle)])
                (set-port-position! handle (+ current amount))
                (Right none)))
        (2 ()
            (Exception 'BadNews "SeekFromEnd not supported" 0))))

(define-unison (getLine.impl.v1 handle)
  (let* ([line (read-line handle)])
    (if (eof-object? line)
        (Right (string->chunked-string ""))
        (Right (string->chunked-string line))
        )))
