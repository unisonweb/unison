; This library is intended to contain the implementation of
; delimited continuations used in the semantics of abilities.

(library (unison cont)
  (export
    prompt
    control)

  (import (chezscheme)
          (unison core))

  ; This implementation is based on the implementation of delimited
  ; continuations used in racket, and makes use of primitives added in
  ; the racket fork of chez scheme.
  ;
  ; The overall idea is to keep track of a meta-continuation that is
  ; made up of a series of captured native continuations. The native
  ; continuations make part of the frames of the meta-continuation,
  ; and these frames can be labeled with prompts to support
  ; multi-prompt delimited continuations. The native 'current
  ; continuation' makes up the portion of the meta-continuation below
  ; the nearest prompt.
  ;
  ; The specific racket-chez feature used is #%$call-in-continuation
  ; which does not seem to be available in the upstream chez. This is
  ; an important feature to have, because the mechanism for obtaining
  ; the native continuation in chez is call/cc, which leaves the
  ; native continuation in place. However, when we capture the native
  ; continuation to push it onto a frame of the meta-continuation, it
  ; may actually be completely eliminated from the implicit
  ; continuation, because we will only ever return to it by popping
  ; the corresponding frame of the meta=continuation.
  ;
  ; Failure to truncate the native continuation can lead to space
  ; leaks due to growing unreachable portions of it. The racket-chez
  ; feature allows us to instead repeatedly replace the implicit
  ; continuation with #%$null-continuation, which avoids the leak.
  (define-virtual-register meta-continuation '())

  ; A record type representing continuation prompts.
  ;
  ; By comparing these records for pointer equality, we can make up
  ; fresh prompts whenever needed, without having to keep track of
  ; some sort of supply of prompts.
  (define-record-type continuation-prompt
    (fields (immutable name)))

  ; A frame of the meta-continuation consists of:
  ;   1. A prompt delimiting the portion of the meta-continuation in
  ;      front of it.
  ;   2. A native continuation to resume when re-entering the given
  ;      frame.
  (define-record-type meta-frame
    (fields
      (immutable prompt)
      (immutable resume-k)))

  ; A convenient abbreviation for grabbing the continuation.
  (define-syntax let/cc
    (syntax-rules ()
      [(let/cc k e ...)
       (identifier? #'k)
       (call/cc (lambda (k) e ...))]))

  ; A wrapper around primitive operations for truncating the implicit
  ; continuation. `h` should be a nullary procedure that we want to
  ; execute in an empty continuation.
  (define (call-in-empty-frame h)
    (($primitive $call-in-continuation)
     ($primitive $null-continuation)
     '() ; marks
     h))

  ; Removes and returns the top frame of the meta-continuation.
  ;
  ; Note: this procedure assumes that the meta-continuation has
  ; already been checked for emptiness, and does no checking of its
  ; own.
  (define (pop-frame!)
    (let ([mf (car meta-continuation)])
      (set! meta-continuation (cdr meta-continuation))
      mf))

  ; Adds a frame to the top of the meta-continuation.
  (define (push-frame! fm)
    (set! meta-continuation (cons fm meta-continuation)))

  ; Handles returning values up the meta-continuation.
  ;
  ; Note: when we replace the native continuation with the null
  ; continuation, for reasons mentioned above, it's important that the
  ; things we run in that null continuation actually call this to
  ; return up the meta-continuation. Otherwise we will _actually_
  ; return to the null continuation, which causes a crash.
  (define (yield-to-meta-continuation results)
    (cond
      [(null? meta-continuation)
       (display "falling off end\n")
       results]
      [else
        (let ([mf (pop-frame!)])
          (($primitive $call-in-continuation)
           (meta-frame-resume-k mf)
           '()
           (lambda ()
             (if (and (pair? results) (null? (cdr results)))
               (car results)
               (apply values results)))))]))

  ; This operation corresponds roughly to `reset` in shift/reset
  ; delimited control. It calls (h p) in a context delimited by
  ; the prompt p.
  ;
  ; This is something of a helper function, as the actual `prompt`
  ; implementation will involve making up a fresh `p`. However,
  ; this common code is useful for test cases using only single
  ; prompt continuations.
  ;
  ; Mechanically, what this does is capture the current native
  ; continuation, push it on the meta-continuation with the specified
  ; prompt attached, and call (h p) in an empty native continuation.
  (define (call-delimited-with-prompt p h)
    (let/cc k
      (call-in-empty-frame
        (lambda ()
          (let-values
            ([results
               (let ([fm (make-meta-frame p k)])
                 (push-frame! fm)
                 (h p))])
            (yield-to-meta-continuation results))))))

  ; Implements prompt for our multi-prompt prompt/control calculus.
  ;
  ; `prompt` makes up a fresh prompt value, and runs its body
  ; delimited with that value, e.g.:
  ;
  ;    (prompt p ...)
  ;
  ; where `p` is a binding for the prompt value. The above is
  ; syntactic sugar for something like:
  ;
  ;    (prompt-impl (lambda (p) ...))
  (define (prompt-impl h)
    (let ([p (make-continuation-prompt 'prompt)])
      (call-delimited-with-prompt p h)))

  ; The nicer syntactic form for the above prompt implementation.
  (define-syntax prompt
    (syntax-rules ()
      [(prompt p e ...)
       (prompt-impl (lambda (p) e ...))]))

  ; Removes the prompt from the first frame of a meta-continuation.
  (define (strip-prompt mc)
    (let ([mf (car mc)])
      (cons (make-meta-frame #f (meta-frame-resume-k mf)) (cdr mc))))

  ; This funcion is used to reinstate a captured continuation. It
  ; should be called with:
  ;
  ;    k - a native continuation to be pushed before the captured
  ;        meta-continuation
  ;    cc - the captured meta-continuation segment
  ;    p - a prompt that should delimit cc
  ;
  ; `p` is used as the prompt value of the `k` frame, so shift/reset
  ; can be implemented by passing the same `p` that was used when `cc`
  ; was captured (as that means that any further `p` control effects
  ; in `cc` do not escape their original scope).
  ;
  ; However, we will usually be calling with p = #f, since shallow
  ; handlers correspond to control effects that are able to eliminate
  ; prompts.
  ;
  ; Note: the captured continuation `cc` is assumed to be in reverse
  ; order, so will be reversed back onto the meta-continuation.
  (define (push-to-meta-continuation k cc p)
    (push-frame! (make-meta-frame p k))
    (let rec ([cc cc])
      (cond
        [(null? cc) #f]
        [else
          (push-frame! (car cc))
          (rec (cdr cc))])))

  ; Wraps a captured continuation with a procedure that reinstates it
  ; upon invocation. This should be called with:
  ;
  ;     ok - the captured native continuation that was captured along
  ;          with...
  ;     cc - the split meta-continuation
  ;     p - a prompt associated with the captured continuation. This
  ;         will be installed as a delimiter when the captured
  ;         continuation is re-pushed. If no delimiting is desired,
  ;         simply use #f, or some dummy prompt that will not be
  ;         involved in actual control flow.
  ;
  ; Note: the captured continuation `cc` is assumed to be in reverse
  ; order, so will be reversed back onto the meta-continuation.
  (define (make-callable-continuation ok cc p)
    (lambda vals
      (let/cc nk
        (($primitive $call-in-continuation)
         ok
         '()
         (lambda ()
           push-to-meta-continuation nk cc p
           (apply values vals))))))

  ; Captures the meta-continuation up to the specified prompt. The
  ; continuation is wrapped in a function that reinstates it when
  ; called. The supplied 'body' `h` is then evaluated with the
  ; captured continuation.
  ;
  ; This implementation is designed to support shallow ability
  ; handlers. This means that we actually implement what would be
  ; called (in delimited continuation literature) control0. This means
  ; that:
  ;
  ;    1. The control operator _removes_ the prompt from the
  ;       meta-continuation. So any control effects referring to the
  ;       same prompt will only be delimited further up the
  ;       continuation.
  ;    2. The procedure reinstating the captured continuation does not
  ;       install a delimiter, so said captured continuation is itself
  ;       a procedure that can have control effects relevant to the
  ;       original prompt.
  ;
  ; The reason for this is that shallow handlers are one-shot in a
  ; corresponding way. They only handle the first effect in their
  ; 'body', and handling _all_ relevant effects requires an explicitly
  ; recursive handler that re-installs a handling delimiter after each
  ; effect request.
  (define (control-impl p h)
    (assert (continuation-prompt? p))
    (let/cc k
      (let rec ([cc '()] [mc meta-continuation])
        (cond
          [(or (null? mc)
               (eq? p (meta-frame-prompt (car mc))))
           (set! meta-continuation (strip-prompt mc))
           (let ([ck (make-callable-continuation k cc #f)])
             (call-in-empty-frame
               (lambda ()
                 (let-values ([results (h ck)])
                   (yield-to-meta-continuation results)))))]
          [else (rec (cons (car mc) cc) (cdr mc))]))))

  ; The nicer syntactic form for the control operator.
  (define-syntax control
    (syntax-rules ()
      [(control p k e ...)
       (control-impl (lambda (k) e ...))]))

  ; TODO: generate this as part of the main program.
  ; (define-init-registers init-regs)
  ; (init-regs)
  )
