; This library exists solely to
; export "string-copy!" for use by "core.ss"
;
; "core.ss" can't require (racket/base) itself,
; as racket/base has name clashes with rnrs
; and the #r6rs language can't handle name
; clashes (the normal racket language can)
(module isolated-string-copy racket
    (provide string-copy!)

    (require racket/base))