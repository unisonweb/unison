#lang racket/base

(require unison/chunked-seq unison/data unison/boot)

(provide
 (rename-out [encodeNat16be unison-FOp-Bytes.encodeNat16be])
 (prefix-out
  unison-FOp-Bytes.
  (combine-out
   decodeNat16be
   decodeNat16le
   decodeNat32be
   decodeNat32le
   decodeNat64be
   decodeNat64le
   encodeNat16be
   encodeNat16le
   encodeNat32be
   encodeNat32le
   encodeNat64be
   encodeNat64le)))

(define none (data (data 'Reference 1 (data 'Id (bytevector 188 183 146 227 128 209 147 130 56 62 78 71 211 90 218 53 77 142 215 206 124 135 78 88 237 141 128 217 73 179 253 41 10 163 9 95 238 250 174 96 226 175 239 183 220 120 200 240 235 114 191 7 78 177 80 174 89 175 167 30 183 5 202 242) 0)) 1))

(define (some-tuple nat bytes)
  (let* ([x13 (data (data 'Reference 1 (data 'Id (bytevector 0 47 241 83 11 67 242 16 233 180 217 202 13 215 57 122 2 198 169 37 176 244 11 203 17 152 115 250 206 243 169 54 56 235 113 254 67 235 139 36 6 123 10 70 60 187 143 28 51 13 72 54 61 54 190 226 97 233 126 39 152 244 205 2) 0)) 0)]
         [x14 (data (data 'Reference 1 (data 'Id (bytevector 21 96 69 68 210 217 186 17 38 195 27 79 14 114 242 130 153 181 84 105 211 85 108 8 22 185 182 47 185 245 151 151 151 106 243 139 166 169 37 212 155 41 240 106 254 77 171 225 209 55 94 95 26 139 140 139 152 70 226 55 62 231 25 245) 0)) 0 bytes x13)]
         [x15 (data (data 'Reference 1 (data 'Id (bytevector 21 96 69 68 210 217 186 17 38 195 27 79 14 114 242 130 153 181 84 105 211 85 108 8 22 185 182 47 185 245 151 151 151 106 243 139 166 169 37 212 155 41 240 106 254 77 171 225 209 55 94 95 26 139 140 139 152 70 226 55 62 231 25 245) 0)) 0 nat x14)]
         [x16 (data (data 'Reference 1 (data 'Id (bytevector 188 183 146 227 128 209 147 130 56 62 78 71 211 90 218 53 77 142 215 206 124 135 78 88 237 141 128 217 73 179 253 41 10 163 9 95 238 250 174 96 226 175 239 183 220 120 200 240 235 114 191 7 78 177 80 174 89 175 167 30 183 5 202 242) 0)) 0 x15)])
    x16))

(define (decodeNatBe bytes size)
  (if (< (chunked-bytes-length bytes) size)
      none
      (let ([buf (chunked-bytes->bytes bytes)])
        (define (loop acc n)
          (when (> n 0)
            ; (bytes-set! buf (- n 1) (bitwise-and acc 255))
            (loop
             (+
              (arithmetic-shift acc 8)
              (bytes-ref buf (- n 1)))
             (- n 1))))
        (some-tuple (loop 0 size) (bytes->chunked-bytes
                                   (subbytes buf size))))))

(define (decodeNatLe bytes size)
  (if (< (chunked-bytes-length bytes) size)
      none
      (let ([buf (chunked-bytes->bytes bytes)])
        (define (loop acc n)
          (when (> n 0)
            ; (bytes-set! buf (- n 1) (bitwise-and acc 255))
            (loop
             (+
              (arithmetic-shift acc 8)
              (bytes-ref buf (- size n)))
             (- n 1))))
        (some-tuple (loop 0 size) (bytes->chunked-bytes
                                   (subbytes buf size))))))

(define (encodeNatBe num size)
  (define buf (make-bytes size 0))
  (define (loop x n)
    (when (> n 0)
      (bytes-set! buf (- n 1) (bitwise-and x 255))
      (loop (arithmetic-shift x -8) (- n 1))))
  (loop num size)
  (bytes->chunked-bytes buf))

(define (encodeNatLe num size)
  (define buf (make-bytes size 0))
  (define (loop x n)
    (when (> n 0)
      (bytes-set! buf (- size n) (bitwise-and x 255))
      (loop (arithmetic-shift x -8) (- n 1))))
  (loop num size)
  (bytes->chunked-bytes buf))

(define (encodeNat16be num) (encodeNatBe num 2))
(define (encodeNat16le num) (encodeNatLe num 2))
(define (encodeNat32be num) (encodeNatBe num 4))
(define (encodeNat32le num) (encodeNatLe num 4))
(define (encodeNat64be num) (encodeNatBe num 8))
(define (encodeNat64le num) (encodeNatLe num 8))

(define (decodeNat16be num) (decodeNatBe num 2))
(define (decodeNat16le num) (decodeNatLe num 2))
(define (decodeNat32be num) (decodeNatBe num 4))
(define (decodeNat32le num) (decodeNatLe num 4))
(define (decodeNat64be num) (decodeNatBe num 8))
(define (decodeNat64le num) (decodeNatLe num 8))
