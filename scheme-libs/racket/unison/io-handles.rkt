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
    getBuffering.impl.v3
    setBuffering.impl.v3
    ))

; Still to implement:
;    handlePosition.impl.v3
;    isSeekable.impl.v3
;    getChar.impl.v1
;    ready.impl.v1
;    isFileOpen.impl.v3
;    isFileEOF.impl.v3
;    setEcho.impl.v1
;    getEcho.impl.v1
;       - unsafe-port->file-descriptor

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

(define BufferMode
    (data 'Reference 1 (data 'Id 0 (bytevector 107 13 114 185 126 64 211 42 13 102 196 109 125 88 217 3 36 251 159 9 35 172 24 16 54 158 72 167 2 22 248 214 77 251 43 81 18 154 173 92 126 242 69 233 142 79 137 22 152 161 71 175 85 193 31 162 82 54 3 70 220 161 142 37) 0)))

(define BlockBuffering (data BufferMode 2))
(define LineBuffering (data BufferMode 1))
(define NoBuffering (data BufferMode 0))

(define-unison (getBuffering.impl.v3 handle)
    (case (file-stream-buffer-mode handle)
        [(none) (Right NoBuffering)]
        [(line) (Right LineBuffering)]
        [(block) (Right BlockBuffering)]
        [(#f) (Exception 'IO "Unable to determine buffering mode of handle")]
        [else (Exception 'IO "Unexpected response from file-stream-buffer-mode")]))

(define-unison (setBuffering.impl.v3 handle mode)
    (data-case mode
        (0 ()
            (file-stream-buffer-mode handle 'none)
            (Right none))
        (1 ()
            (file-stream-buffer-mode handle 'line)
            (Right none))
        (2 ()
            (file-stream-buffer-mode handle 'block)
            (Right none))
        (3 (size)
            (Exception 'IO "Sized block buffering not supported"))))

