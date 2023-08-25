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
         (only-in unison/core bytevector->base32-string b32h)
         unison/data
         unison/data-info
         unison/chunked-seq
         (for-syntax racket/base unison/data-info))

(provide
  builtin-Value.value
  unison-POp-CACH
  unison-POp-LOAD)

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
               [#,unison-schemeterm-sexpr:tag (tms)
                (map decode-term (chunked-list->list tms))]
               [#,unison-schemeterm-handle:tag (as h tms)
                 `(handle
                    ,(map
                       (lambda (tx) `(quote ,(text->ident tx)))
                       (chunked-list->list as))
                    ,(text->ident h)
                    ,@(map decode-term (chunked-list->list tms)))]
               [#,unison-schemeterm-cases:tag (hd sc cs)
                 `(,(text->ident hd)
                    ,(decode-term sc)
                    ,@(map decode-term (chunked-list->list cs)))]
               [#,unison-schemeterm-binds:tag (hd bs bd)
                 `(,(text->ident hd)
                    ,(map decode-binding (chunked-list->list bs))
                    ,(decode-term bd))]
               [#,unison-schemeterm-ident:tag (tx) (text->ident tx)]
               [#,unison-schemeterm-string:tag (tx)
                (chunked-string->string tx)]
               [#,unison-schemeterm-symbol:tag (tx)
                `(quote ,(text->ident tx))]
               [#,unison-schemeterm-bytevec:tag (ns) (bytevector ns)]
               [else
                 (raise
                   (format
                     "decode-term: unimplemented case: ~a"
                     tm))]))

           (data-case dfn
             [#,unison-schemedefn-define:tag (nm vs bd)
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

(define (decode-ref rf)
  (match rf
    [(unison-data r t (list name))
     #:when (= t unison-reference-builtin:tag)
     (sum 0 (chunked-string->string name))]
    [(unison-data r t (list id))
     #:when (= t unison-reference-derived:tag)
     (data-case id
       [0 (bs i) (sum 1 bs i)])]))

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
             [#,unison-schemeterm-ident:tag (name) name]
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
  (match tl
    [(unison-termlink-con r i)
     (raise "termlink->name: data constructor")]
    [(unison-termlink-builtin name)
     (string-append "builtin-" name)]
    [(unison-termlink-derived bs i)
     (let ([hs (bytevector->base32-string b32h bs)]
           [po (if (= i 0) "" (string-append "." (number->string i)))])
       (string->symbol
         (string-append "ref-" (substring hs 0 8) po)))]))

(define (ref-bytes r)
  (sum-case (decode-ref r)
    [0 (tx) (raise (string-append "ref-bytes: builtin ref: " tx))]
    [1 (bs i) bs]))

(define (termlink-bytes tl)
  (match tl
    [(unison-termlink-derived bs i) bs]
    [(unison-termlink-builtin name)
     (raise (string-append "termlink-bytes: builtin ref: " name))]
    [(unison-termlink-con r i)
     (raise (string-append
              "termlink-bytes: called with constructor link"))]))

(define (termlink->reference rn)
  (match rn
    [(unison-termlink-builtin name)
     (unison-reference-builtin
       (string->chunked-string name))]
    [(unison-termlink-derived bs i)
     (unison-reference-derived (unison-id-id bs i))]
    [else (raise "termlink->reference: con case")]))

(define (group-reference gr)
  (data-case gr
    [0 (r _) r]))

(define runtime-namespace
  (let ([ns (variable-reference->namespace (#%variable-reference))])
    (namespace-require ''#%kernel ns)
    ns))

(define runtime-module-map (make-hash))

(define (reflect-derived bs i)
  (data unison-reference:link unison-reference-derived:tag
    (data unison-id:link unison-id-id:tag bs i)))

(define (function->groupref f)
  (match (lookup-function-link f)
    [(unison-termlink-derived h i)
     (unison-groupref-group
       (unison-reference-derived
         (unison-id-id h i)))]))

(define (reify-vlit vl)
  (match vl
    [(unison-data _ t (list l))
     (cond
       [(= t unison-vlit-bytes:tag) l]
       [(= t unison-vlit-bytearray:tag) l]
       [(= t unison-vlit-text:tag) l]
       [(= t unison-vlit-typelink:tag) (reference->typelink l)]
       [(= t unison-vlit-termlink:tag) (referent->termlink l)]
       [(= t unison-vlit-code:tag) (unison-code l)]
       [(= t unison-vlit-quote:tag) (unison-quote l)]
       [(= t unison-vlit-seq:tag)
        ; TODO: better map over chunked list
        (vector->chunked-list
          (vector-map reify-value (chunked-list->vector l)))]
       [else
         (raise (format "decode-vlit: unimplemented case: !a" vl))])]))

(define (reify-value v)
  (match v
    [(unison-data _ t (list rf rt us0 bs0))
     #:when (= t unison-value-data:tag)
     (let ([us (chunked-list->list us0)]
           [bs (map reify-value (chunked-list->list bs0))])
       (cond
         [(null? us) (make-data (reference->typelink rf) rt bs)]
         [(and (null? bs) (= 1 (length us))) (car us)]
         [else
           (raise
             (format
               "reify-value: unimplemented data case: ~a"
               (describe-value v)))]))]
    [(unison-data _ t (list gr us0 bs0))
     #:when (= t unison-value-partial:tag)
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
     #:when (= t unison-value-vlit:tag)
     (reify-vlit vl)]
    [(unison-data _ t (list us0 bs0 k))
     #:when (= t unison-value-cont:tag)
     (raise "reify-value: unimplemented cont case")]
    [(unison-data r t fs)
     (raise "reify-value: unimplemented data case")]
    [else
      (raise (format "reify-value: unknown tag"))]))

(define (reflect-typelink tl)
  (match tl
    [(unison-typelink-builtin name) (unison-reference-builtin name)]
    [(unison-typelink-derived h i)
     (unison-reference-derived (unison-id-id h i))]))

(define (reflect-termlink tl)
  (match tl
    [(unison-termlink-con r i)
     (unison-referent-con (reflect-typelink r) i)]
    [(unison-termlink-builtin name)
     (unison-referent-def (unison-reference-builtin name))]
    [(unison-termlink-derived h i)
     (unison-referent-def
       (unison-reference-derived
         (unison-id-id h i)))]))

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
    [(? unison-termlink?)
     (unison-value-vlit (unison-vlit-termlink (reflect-termlink v)))]
    [(? unison-typelink?)
     (unison-value-vlit (unison-vlit-typelink (reflect-typelink v)))]
    [(unison-code sg) (unison-value-vlit (unison-vlit-code sg))]
    [(unison-quote q) (unison-value-vlit (unison-vlit-quote q))]
    [(unison-closure f as)
     (unison-value-partial
       (function->groupref f)
       empty-chunked-list
       (list->chunked-list (map reflect-value as)))]
    [(? procedure?)
     (unison-value-partial
       (function->groupref v)
       empty-chunked-list
       empty-chunked-list)]
    [(unison-data rf t fs)
     (unison-value-data
       (reflect-typelink rf)
       t
       empty-chunked-list
       (list->chunked-list (map reflect-value fs)))]))

(define-unison
  (builtin-Value.value v)
  (let ([rv (reflect-value v)])
    (unison-quote rv)))

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
    (match tl
      [(unison-termlink-con r t)
       (raise "CACH: trying to add code for data constructor")]
      [(unison-termlink-builtin name)
       (raise "CACH: trying to add code for a builtin")]
      [(unison-termlink-derived bs i)
       (let ([sg (unison-code-rep co)]
             [r (reflect-derived bs i)])
         (map decode-syntax
              (chunked-list->list (gen-scheme r sg))))])))

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
     (let ([sym (group-ref-sym gr)]
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

(define (code-dependencies co)
  (group-term-dependencies (unison-code-rep co)))

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
              [refs (map termlink->reference links)]
              [deps (flat-map code-dependencies udefs)]
              [fdeps (filter need-dependency? deps)]
              [rdeps (remove* refs fdeps)])
         (if (null? rdeps)
           (let ([sdefs (flatten (map gen-code udefs))]
                 [mname (generate-module-name links)])
             (add-module-associations links mname)
             (add-runtime-module mname links sdefs)
             (sum 0 '()))
           (sum 1 (list->chunked-list rdeps))))]
      [else (sum 0 '())])))

(define (unison-POp-LOAD v0)
  (let* ([val (unison-quote-val v0)]
         [deps (value-term-dependencies val)]
         [fldeps (chunked-list->list deps)]
         [fdeps (filter need-dependency? (chunked-list->list deps))])
    (if (null? fdeps)
      (sum 1 (reify-value val))
        (sum 0 (list->chunked-list fdeps)))))
