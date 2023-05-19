; This library implements primops that require unison-generated
; code as a basis. It imports the boot-generated library which is
; itself allowed to import the ordinary primops module for running
; unison code. Then the surface code can import _both_ sets of
; primop moduels for the purpose of wrapping them into surface unison
; code.
#!racket/base
(require racket/base
         racket/vector
         unison/boot
         unison/boot-generated
         unison/chunked-seq
         (for-syntax racket/base unison/boot-generated))

(provide unison-POp-CACH unison-POp-LOAD)

(define (chunked-list->list cl)
  (vector->list (chunked-list->vector cl)))

(define-syntax make-syntax-decoder
  (lambda stx
    (syntax-case stx ()
      [(make-syntax-decoder)
       #`(lambda (dfn)
           (define (text->ident tx)
             (let* ([st (chunked-string->string tx)]
                    [n (string->number st)])
               (if n n (string->symbol st))))

           (define (decode-binding bn)
             (data-case bn
               [0 (nm rest)
                 (data-case rest
                   [0 (tm nil) (list (text->ident nm) (decode-term tm))])]
               [else
                 (raise
                   (format
                     "decode-binding: unimplemented case: ~a"
                     bn))]))

           (define (decode-term tm)
             (data-case tm
               [#,(sexpr-tag) (tms)
                (map decode-term (chunked-list->list tms))]
               [#,(handle-tag) (as h tms)
                 `(handle
                    ,(map
                       (lambda (tx) `(quote ,(text->ident tx)))
                       (chunked-list->list as))
                    ,(text->ident h)
                    ,@(map decode-term (chunked-list->list tms)))]
               [#,(cases-tag) (hd sc cs)
                 `(,(text->ident hd)
                    ,(decode-term sc)
                    ,@(map decode-term (chunked-list->list cs)))]
               [#,(binds-tag) (hd bs bd)
                 `(,(text->ident hd)
                    ,(map decode-binding (chunked-list->list bs))
                    ,(decode-term bd))]
               [#,(ident-tag) (tx) (text->ident tx)]
               [#,(string-tag) (tx)
                `(string->chunked-string
                   ,(chunked-string->string tx))]
               [#,(symbol-tag) (tx) `(quote ,(text->ident tx))]
               [#,(bytevec-tag) (ns) (bytevector ns)]
               [else
                 (raise
                   (format
                     "decode-term: unimplemented case: ~a"
                     tm))]))

           (data-case dfn
             [0 (nm vs bd)
               (let ([head (map text->ident
                                (cons nm (chunked-list->list vs)))]
                     [body (decode-term bd)])
                 (list 'define-unison head body))]
             [else
               (raise
                 (format
                   "decode-syntax: unimplemented case: ~a"
                   dfn))]))])))

(define decode-syntax0 (make-syntax-decoder))

(define (decode-syntax v)
  (let ([stx (decode-syntax0 v)])
    (display (format "~a\n\n" stx))
    stx))

(define-syntax make-ref-decoder
  (lambda (stx)
    (syntax-case stx ()
      [(make-ref-decoder)
       #`(lambda (rf)
           (data-case rf
             [#,(builtin-tag) (tx) (sum 0 (chunked-string->string tx))]
             [#,(derived-tag) (id)
               (data-case id [0 (bs i) (sum 1 bs i)])]))])))

(define decode-ref (make-ref-decoder))

(define-syntax make-termlink-decoder
  (lambda (stx)
    (syntax-case stx ()
      [(_)
       #`(lambda (tl)
           (data-case tl
             ; TODO: generate this programatically
             [0 (rf i) (sum 0 rf i)] ; con case
             [1 (rf) (sum 1 rf)]))]))) ; ref case

(define decode-termlink (make-termlink-decoder))

(define-syntax make-group-ref-decoder
  (lambda (stx)
    (syntax-case stx ()
      [(_)
       #`(lambda (gr)
           (data-case (group-ref-ident gr)
             [#,(ident-tag) (name) name]
             [else
               (raise
                 (format
                   "decode-group-ref: unimplemented data case: ~a"
                   (describe-value gr)))]))])))

(define decode-group-ref (make-group-ref-decoder))
(define (group-ref-sym gr)
  (string->symbol
    (chunked-string->string
      (decode-group-ref gr))))

(define (termlink->name tl)
  (sum-case (decode-termlink tl)
    [0 (rf i) (raise "termlink->name: data constructor")]
    [1 (rf) (string->symbol
              (chunked-string->string
                (ref-ident rf)))]))

(define (ref-bytes r)
  (sum-case (decode-ref r)
    [0 (tx) (raise (string-append "ref-bytes: builtin ref: " tx))]
    [1 (bs i) bs]))

(define (termlink-bytes tl)
  (sum-case (decode-termlink tl)
    [0 (rf i) (raise "termlink-bytes: called with constructor link")]
    [1 (rf) (ref-bytes rf)]))

(define (group-reference gr)
  (data-case gr
    [0 (r _) r]))

(define runtime-namespace
  (let ([ns (variable-reference->namespace (#%variable-reference))])
    (namespace-require ''#%kernel ns)
    (namespace-require 'unison/builtin-generated ns)
    ns))

(define runtime-module-map (make-hash))

(define-syntax make-value-decoder
  (lambda (stx)
    (syntax-case stx ()
      [(make-value-decoder)
       #`(lambda (val)
           (define (decode-vlit vl)
             (data-case vl
               [#,(lit-bytes-tag) (bs) bs]
               [#,(lit-bytearray-tag) (bs) bs]
               [#,(lit-text-tag) (tx) tx]
               [#,(lit-typelink-tag) (tl) tl]
               [#,(lit-termlink-tag) (tl) tl]
               [#,(lit-code-tag) (sg) sg]
               [#,(lit-value-tag) (tx) tx]
               [#,(lit-seq-tag) (vs)
                (vector->chunked-list
                  (vector-map
                    decode-val
                    (chunked-list->vector vs)))]
               [else
                 (raise
                   (format
                     "decode-vlit: unimplemented case: ~a"
                     vl))]))

           (define (decode-val v)
             (data-case v
               [#,(data-tag) (rf t us0 bs0)
                (let ([us (chunked-list->list us0)]
                      [bs (map decode-val (chunked-list->list bs0))])
                  (cond
                    [(null? us) (apply data rf t bs)]
                    [(and (null? bs) (= 1 (length us))) (car us)]
                    [else
                      (raise
                        (format
                          "decode-val: unimplemented data case: ~a"
                          (describe-value v)))]))]
               [#,(partial-tag) (gr us0 bs0)
                (let ([us (chunked-list->list us0)]
                      [bs (map decode-val (chunked-list->list bs0))])
                  (cond
                    [(null? us)
                     (let ([proc (resolve-proc gr)])
                       (apply proc bs))]
                    [else
                      (raise
                        "decode-val: unimplemented partial application case")]))]
               [#,(vlit-tag) (vl) (decode-vlit vl)]
               [#,(cont-tag) (us0 bs0 k)
                (raise "decode-val: unimplemented cont case")]
               [else
                 (raise "decode-val: unknown tag")]))

           (decode-val val))])))

(define reify-value (make-value-decoder))

(define (ufst utup)
  (data-case utup
    [0 (fst _) fst]))

(define (usnd utup)
  (data-case utup
    [0 (_ rest)
      (data-case rest
        [0 (snd _) snd])]))

(define (splat-upair utup)
  (data-case utup
    [0 (fst rest)
      (data-case rest
        [0 (snd nil)
          (values fst snd)])]))

(define (gen-code args)
  (let-values ([(tl co) (splat-upair args)])
    (data-case tl
      [0 (r i)
        (raise "CACH: trying to add code for data constructor")]
      [1 (r) (map decode-syntax
                  (chunked-list->list (gen-scheme r co)))])))

(define (flatten ls)
  (cond
    [(null? ls) '()]
    [else (append (car ls) (flatten (cdr ls)))]))

(define module-count 0)

(define (fresh-module-name)
  (let ([n module-count])
    (set! module-count (+ n 1))
    (string-append "runtime-module-" (number->string n))))

(define (generate-module-name links)
  (if (null? links)
    (raise "could not generate module name for dynamic code")
    (let* ([top (car links)]
           [bs (termlink-bytes top)]
           [ebs (fresh-module-name)])
      (if (hash-has-key? runtime-module-map bs)
        (generate-module-name (cdr links))
        (string->symbol ebs)))))

(define (add-module-associations links mname)
  (for-each
    (lambda (link)
      (let ([bs (termlink-bytes link)])
        (if (hash-has-key? runtime-module-map bs)
          #f
          (hash-set! runtime-module-map bs mname))))
    links))

(define (resolve-proc gr)
  (sum-case (decode-ref (group-reference gr))
    [0 (tx)
     (parameterize ([current-namespace runtime-namespace])
       (dynamic-require
         'unison/builtin-generated
         (string->symbol (string-append "builtin-" tx))))]
    [1 (bs i)
     (let* ([sym (group-ref-sym gr)]
           [mname (hash-ref runtime-module-map bs)])
       (parameterize ([current-namespace runtime-namespace])
         (dynamic-require `(quote ,mname) sym)))]))

(define (add-runtime-module mname links defs)
  (let ([names (map termlink->name links)])
    (eval
      `(module ,mname racket/base
         (require unison/boot)
         (require unison/primops)
         (require unison/primops-generated)
         (require unison/builtin-generated)
         (provide ,@names)
         ,@defs)
      runtime-namespace)))

; TODO: check dependencies and indicate problems.
(define (unison-POp-CACH dfns0)
  (let ([udefs (chunked-list->list dfns0)])
    (cond
      [(not (null? udefs))
       (let* ([links (map ufst udefs)]
              [sdefs (flatten (map gen-code udefs))]
              [mname (generate-module-name links)])
         (add-module-associations links mname)
         (add-runtime-module mname links sdefs))])
    (sum 0 '())))

; TODO: check dependencies and indicate any problems.
(define (unison-POp-LOAD val) (sum 1 (reify-value val)))
