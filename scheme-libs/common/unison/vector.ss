
(library (unison vector)
  (export
    freeze-vector!)

  (import (chezscheme))

  (define (freeze-vector! vec)
    (($primitive $vector-set-immutable!) vec)))
