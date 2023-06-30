; This library implements primops that require unison-generated
; code as a basis. It imports the boot-generated library which is
; itself allowed to import the ordinary primops module for running
; unison code. Then the surface code can import _both_ sets of
; primop moduels for the purpose of wrapping them into surface unison
; code.
#!racket/base
(require (except-in racket false true unit any)
         racket/vector
         unison/boot
         unison/boot-generated
         unison/data
         unison/data-info
         unison/chunked-seq
         (for-syntax racket/base unison/data-info))

(provide unison-POp-CACH unison-POp-LOAD)

(define (chunked-list->list cl)
  (vector->list (chunked-list->vector cl)))

(define (list->chunked-list l)
  (vector->chunked-list (list->vector l)))

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
               [#,unison-schemeterm-sexpr-tag (tms)
                (map decode-term (chunked-list->list tms))]
               [#,unison-schemeterm-handle-tag (as h tms)
                 `(handle
                    ,(map
                       (lambda (tx) `(quote ,(text->ident tx)))
                       (chunked-list->list as))
                    ,(text->ident h)
                    ,@(map decode-term (chunked-list->list tms)))]
               [#,unison-schemeterm-cases-tag (hd sc cs)
                 `(,(text->ident hd)
                    ,(decode-term sc)
                    ,@(map decode-term (chunked-list->list cs)))]
               [#,unison-schemeterm-binds-tag (hd bs bd)
                 `(,(text->ident hd)
                    ,(map decode-binding (chunked-list->list bs))
                    ,(decode-term bd))]
               [#,unison-schemeterm-ident-tag (tx) (text->ident tx)]
               [#,unison-schemeterm-string-tag (tx)
                `(string->chunked-string
                   ,(chunked-string->string tx))]
               [#,unison-schemeterm-symbol-tag (tx)
                `(quote ,(text->ident tx))]
               [#,unison-schemeterm-bytevec-tag (ns) (bytevector ns)]
               [else
                 (raise
                   (format
                     "decode-term: unimplemented case: ~a"
                     tm))]))

           (data-case dfn
             [#,unison-schemedefn-define-tag (nm vs bd)
               (let ([head (map text->ident
                                (cons nm (chunked-list->list vs)))]
                     [body (decode-term bd)])
                 (list 'define-unison head body))]
             [else
               (raise
                 (format
                   "decode-syntax: unimplemented case: ~a"
                   dfn))]))])))

(define decode-syntax (make-syntax-decoder))

(define-syntax make-ref-decoder
  (lambda (stx)
    (syntax-case stx ()
      [(make-ref-decoder)
       #`(lambda (rf)
           (data-case rf
             [#,unison-reference-builtin-tag (tx)
              (sum 0 (chunked-string->string tx))]
             [#,unison-reference-derived-tag (id)
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

(define (termlink-ref tl)
  (sum-case (decode-termlink tl)
    [0 (rf i) (raise
                (format
                  "termlink-ref: unexpected constructor reference"
                  (describe-value tl)))]
    [1 (rf) rf]))

(define-syntax make-group-ref-decoder
  (lambda (stx)
    (syntax-case stx ()
      [(_)
       #`(lambda (gr)
           (data-case (group-ref-ident gr)
             [#,unison-schemeterm-ident-tag (name) name]
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
    ns))

(define runtime-module-map (make-hash))

(define (reify-reference rf)
  (match rf
    [(unison-data _ t (list nm))
     #:when (= t unison-reference-builtin-tag)
     (unison-typelink-builtin (chunked-string->string nm))]
    [(unison-data _ t (list id))
     #:when (= t unison-reference-derived-tag)
     (match id
       [(unison-data _ t (list rf i))
        #:when (= t unison-id-id-tag)
        (unison-typelink-derived rf i)])]))

(define (reify-referent rn)
  (match rn
    [(unison-data _ t (list rf i))
     #:when (= t unison-referent-con-tag)
     (unison-termlink-con (reify-reference rf) i)]
    [(unison-data _ t (list rf))
     #:when (= t unison-referent-def-tag)
     (unison-termlink-ref (reify-reference rf))]))

(define (reify-vlit vl)
  (match vl
    [(unison-data _ t (list l))
     (cond
       [(= t unison-vlit-bytes-tag) l]
       [(= t unison-vlit-bytearray-tag) l]
       [(= t unison-vlit-text-tag) l]
       [(= t unison-vlit-typelink-tag) (reify-reference l)]
       [(= t unison-vlit-termlink-tag) (reify-referent l)]
       [(= t unison-vlit-code-tag) (unison-code l)]
       [(= t unison-vlit-quote-tag) (unison-quote l)]
       [(= t unison-vlit-seq-tag)
        ; TODO: better map over chunked list
        (vector->chunked-list
          (vector-map reify-value (chunked-list->vector l)))]
       [else
         (raise (format "decode-vlit: unimplemented case: !a" vl))])]))

(define (reify-value v)
  (match v
    [(unison-data _ t (list rf t us0 bs0))
     #:when (= t unison-value-data-tag)
     (let ([us (chunked-list->list us0)]
           [bs (map reify-value (chunked-list->list bs0))])
       (cond
         [(null? us) (make-data (reify-reference rf) t bs)]
         [(and (null? bs) (= 1 (length us))) (car us)]
         [else
           (raise
             (format
               "reify-value: unimplemented data case: ~a"
               (describe-value v)))]))]
    [(unison-data _ t (list gr us0 bs0))
     #:when (= t unison-value-partial-tag)
     (let ([us (chunked-list->list us0)]
           [bs (map reify-value (chunked-list->list bs0))])
       (cond
         [(null? us)
          (let ([proc (resolve-proc gr)])
            (apply proc bs))]
         [else
           (raise
             "reify-value: unimplemented partial application case")]))]
    [(unison-data _ t (list vl))
     #:when (= t unison-value-vlit-tag)
     (reify-vlit vl)]
    [(unison-data _ t (list us0 bs0 k))
     (raise "reify-value: unimplemented cont case")]
    [else
      (raise "reify-value: unknown tag")]))

; (define-syntax make-value-decoder
;   (lambda (stx)
;     (syntax-case stx ()
;       [(make-value-decoder)
;        #`(lambda (val)
;            (define (decode-vlit vl)
;              (data-case vl
;                [#,unison-vlit-bytes-tag (bs) bs]
;                [#,unison-vlit-bytearray-tag (bs) bs]
;                [#,unison-vlit-text-tag (tx) tx]
;                [#,unison-vlit-typelink-tag (tl)
;                  (data-case tl
;                    [#,unison-reference-builtin-tag (nm)
;                     (unison-typelink-builtin
;                       (chunked-string->string nm))]
;                    [#,unison-reference-derived-tag (id)
;                      (data-case id
;                        [#,unison-id-id-tag (rf i)
;                          (unison-typelink-derived rf)])])]
;                [#,unison-vlit-termlink-tag (tl)
;                 (data-case tl
;                   [#,)]
;                [#,unison-vlit-code-tag (sg) sg]
;                [#,unison-vlit-quote-tag (vl) vl]
;                [#,unison-vlit-seq-tag (vs)
;                 (vector->chunked-list
;                   (vector-map
;                     decode-val
;                     (chunked-list->vector vs)))]
;                [else
;                  (raise
;                    (format
;                      "decode-vlit: unimplemented case: ~a"
;                      vl))]))
;
;            (define (decode-val v)
;              (data-case v
;                [#,unison-value-data-tag (rf t us0 bs0)
;                 (let ([us (chunked-list->list us0)]
;                       [bs (map decode-val (chunked-list->list bs0))])
;                   (cond
;                     [(null? us) (apply data rf t bs)]
;                     [(and (null? bs) (= 1 (length us))) (car us)]
;                     [else
;                       (raise
;                         (format
;                           "decode-val: unimplemented data case: ~a"
;                           (describe-value v)))]))]
;                [#,unison-value-partial-tag (gr us0 bs0)
;                 (let ([us (chunked-list->list us0)]
;                       [bs (map decode-val (chunked-list->list bs0))])
;                   (cond
;                     [(null? us)
;                      (let ([proc (resolve-proc gr)])
;                        (apply proc bs))]
;                     [else
;                       (raise
;                         "decode-val: unimplemented partial application case")]))]
;                [#,unison-value-vlit-tag (vl) (decode-vlit vl)]
;                [#,unison-value-cont-tag (us0 bs0 k)
;                 (raise "decode-val: unimplemented cont case")]
;                [else
;                  (raise "decode-val: unknown tag")]))
;
;            (decode-val val))])))

; (define reify-value (make-value-decoder))

(define (reflect-value v)
  (match v
    [(? chunked-bytes?)
     (unison-value-vlit (unison-vlit-bytes v))]
    [(? bytes?)
     (unison-value-vlit (unison-vlit-bytearray v))]
    [(? chunked-string?)
     (unison-value-vlit (unison-vlit-text v))]
    ; TODO: better map over chunked lists
    [(? chunked-list?)
     (unison-value-vlit
       (unison-vlit-seq
         (list->chunked-list
           (map reflect-value (chunked-list->list v)))))]
    ; [(? unison-termlink?)
    ;  (unison-value-vlit
    ;    (unison-vlit-termlink
    [(unison-data rf t fs)
     (unison-value-data
       rf t
       empty-chunked-list
       (list->chunked-list (map reflect-value fs)))]))

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

(define (need-dependency? d)
  (data-case d
    [0 (tx) #f] ; builtin
    [1 (id)
     (data-case id
       [0 (bs i)
        (not (hash-has-key? runtime-module-map bs))])]))

(define (resolve-proc gr)
  (sum-case (decode-ref (group-reference gr))
    [0 (tx)
     (parameterize ([current-namespace runtime-namespace])
       (dynamic-require
         'unison/simple-wrappers
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
         (require unison/simple-wrappers)
         (provide ,@names)
         ,@defs)
      runtime-namespace)))

(define (unison-POp-CACH dfns0)
  (define (flat-map f l)
    (foldl
      (lambda (x acc)
        (append (chunked-list->list (f (usnd x))) acc))
      '()
      l))

  (let ([udefs (chunked-list->list dfns0)])
    (cond
      [(not (null? udefs))
       (let* ([links (map ufst udefs)]
              [refs (map termlink-ref links)]
              [deps (flat-map group-term-dependencies udefs)]
              [fdeps (filter need-dependency? deps)]
              [rdeps (remove* refs fdeps)])
         (display (describe-value links)) (display "\n\n")
         (display (describe-value deps)) (display "\n\n")
         (if (null? rdeps)
           (let ([sdefs (flatten (map gen-code udefs))]
                 [mname (generate-module-name links)])
             (add-module-associations links mname)
             (add-runtime-module mname links sdefs)
             (sum 0 '()))
           (sum 1 (list->chunked-list rdeps))))]
      [else (sum 0 '())])))

(define (unison-POp-LOAD val)
  (let* ([deps (value-term-dependencies val)]
         [fdeps (filter need-dependency? (chunked-list->list deps))])
    (if (null? fdeps)
      (sum 1 (reify-value val))
      (sum 0 (list->chunked-list fdeps)))))
