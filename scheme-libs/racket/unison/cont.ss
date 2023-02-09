
#!r6rs
(library (unison cont)
  (export
    abort-to
    make-prompt
    prompt0-at
    control0-at)

  (import
    (rnrs)
    (unison core)

    (rename
      (only (racket)
            make-continuation-prompt-tag)
      (make-continuation-prompt-tag make-prompt))

    (rename
      (only (racket control)
            abort/cc
            prompt0-at
            control0-at)
      (abort/cc abort-to))))
