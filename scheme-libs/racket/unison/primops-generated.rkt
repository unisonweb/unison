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
  builtin-Value.value:termlink
  builtin-Value.reflect
  builtin-Value.reflect:termlink
  builtin-Code.isMissing
  builtin-Code.isMissing:termlink
  builtin-Code.lookup
  builtin-Code.lookup:termlink
  builtin-validateSandboxed
  builtin-validateSandboxed:termlink
  builtin-Value.validateSandboxed
  builtin-Value.validateSandboxed:termlink
  builtin-sandboxLinks
  builtin-sandboxLinks:termlink

  builtin-Code.deserialize:termlink
  builtin-Code.serialize:termlink
  builtin-Code.validateLinks:termlink
  builtin-Value.deserialize:termlink
  builtin-Value.serialize:termlink
  builtin-crypto.hash:termlink
  builtin-crypto.hmac:termlink

  unison-POp-CACH
  unison-POp-LOAD
  unison-POp-LKUP

  ; some exports of internal machinery for use elsewhere
  gen-code
  reify-value
  termlink->name

  add-runtime-code
  build-intermediate-expressions
  build-runtime-module
  termlink->proc)

(define-builtin-link Value.value)
(define-builtin-link Value.reflect)
(define-builtin-link Code.isMissing)
(define-builtin-link Code.lookup)

(define-builtin-link Code.deserialize)
(define-builtin-link Code.serialize)
(define-builtin-link Code.validateLinks)
(define-builtin-link Value.deserialize)
(define-builtin-link Value.serialize)
(define-builtin-link crypto.hash)
(define-builtin-link crypto.hmac)
(define-builtin-link validateSandboxed)
(define-builtin-link Value.validateSandboxed)
(define-builtin-link sandboxLinks)

(define (chunked-list->list cl)
  (vector->list (chunked-list->vector cl)))

(define (list->chunked-list l)
  (vector->chunked-list (list->vector l)))

(define (assemble-cases hd sc cs)
  (cond
    [(equal? hd 'cond) `(cond ,@cs)]
    [else `(,hd ,sc ,@cs)]))

(define (decode-term tm)
  (match tm
    [(unison-data _ t (list tms))
     #:when (= t unison-schemeterm-sexpr:tag)
     (map decode-term (chunked-list->list tms))]
    [(unison-data _ t (list as h tms))
     #:when (= t unison-schemeterm-handle:tag)
     `(handle
        ,(map
           (lambda (tx) `(quote ,(text->ident tx)))
           (chunked-list->list as))
        ,(text->ident h)
        ,@(map decode-term (chunked-list->list tms)))]
    [(unison-data _ t (list hd sc cs))
     #:when (= t unison-schemeterm-cases:tag)
     (assemble-cases
       (text->ident hd)
       (decode-term sc)
       (map decode-term (chunked-list->list cs)))]
    [(unison-data _ t (list hd bs bd))
     #:when (= t unison-schemeterm-binds:tag)
     `(,(text->ident hd)
        ,(map decode-binding (chunked-list->list bs))
        ,(decode-term bd))]
    [(unison-data _ t (list tx))
     #:when (= t unison-schemeterm-ident:tag)
     (text->ident tx)]
    [(unison-data _ t (list tx))
     #:when (= t unison-schemeterm-string:tag)
     (chunked-string->string tx)]
    [(unison-data _ t (list tx))
     #:when (= t unison-schemeterm-symbol:tag)
     `(quote ,(text->ident tx))]
    [(unison-data _ t (list ns))
     #:when (= t unison-schemeterm-bytevec:tag)
     (list->bytes (chunked-list->list ns))]
    [else
      (raise (format "decode-term: unimplemented case: ~a" tm))]))

(define (decode-binding bn)
  (data-case bn
    [0 (nm rest)
      (data-case rest
        [0 (tm nil) (list (text->ident nm) (decode-term tm))])]
    [else
      (raise
        (format "decode-binding: unimplemented case: ~a" bn))]))

(define (decode-syntax dfn)
  (match dfn
    [(unison-data _ t (list nm vs bd))
     #:when (= t unison-schemedefn-define:tag)
     (let ([head (map text->ident
                      (cons nm (chunked-list->list vs)))]
           [body (decode-term bd)])
       (list 'define-unison head body))]
    [(unison-data _ t (list nm bd))
     #:when (= t unison-schemedefn-alias:tag)
     (list 'define (text->ident nm) (decode-term bd))]
    [else
      (raise (format "decode-syntax: unimplemented case: ~a" dfn))]))

(define (string->char st)
  (cond
    [(< (string-length st) 3) #f]
    [(> (string-length st) 3) #f]
    [(equal? (substring st 0 2) "#\\") (string-ref st 2)]
    [else #f]))

(define (text->ident tx)
  (let* ([st (chunked-string->string tx)]
         [n (string->number st)]
         [c (string->char st)])
    (cond
      [(equal? st "#f") #f]
      [(equal? st "#t") #t]
      [c c]
      [n n]
      [else (string->symbol st)])))

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
         (unison-id-id h i))
       0)]
    [(unison-termlink-builtin name)
     (unison-groupref-group
       (unison-reference-builtin (string->chunked-string name))
       0)]
    [else (raise "function->groupref: con case")]))

(define (reify-vlit vl)
  (match vl
    [(unison-data _ t (list l))
     (cond
       [(= t unison-vlit-bytes:tag) l]
       [(= t unison-vlit-char:tag) l]
       [(= t unison-vlit-bytearray:tag) l]
       [(= t unison-vlit-text:tag) l]
       [(= t unison-vlit-termlink:tag) (referent->termlink l)]
       [(= t unison-vlit-typelink:tag) (reference->typelink l)]
       [(= t unison-vlit-float:tag) l]
       [(= t unison-vlit-pos:tag) l]
       [(= t unison-vlit-neg:tag) (- l)]
       [(= t unison-vlit-quote:tag) (unison-quote l)]
       [(= t unison-vlit-code:tag) (unison-code l)]
       [(= t unison-vlit-array:tag) (vector-map reify-value l)]
       [(= t unison-vlit-seq:tag)
        ; TODO: better map over chunked list
        (vector->chunked-list
          (vector-map reify-value (chunked-list->vector l)))]
       [else
         (raise (format "decode-vlit: unimplemented case: !a" vl))])]))

(define (reify-value v)
  (match v
    [(unison-data _ t (list rf rt bs0))
     #:when (= t unison-value-data:tag)
     (let ([bs (map reify-value (chunked-list->list bs0))])
       (make-data (reference->typelink rf) rt bs))]
    [(unison-data _ t (list gr bs0))
     #:when (= t unison-value-partial:tag)
     (let ([bs (map reify-value (chunked-list->list bs0))]
           [proc (resolve-proc gr)])
       (apply proc bs))]
    [(unison-data _ t (list vl))
     #:when (= t unison-value-vlit:tag)
     (reify-vlit vl)]
    [(unison-data _ t (list bs0 k))
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

(define (number-reference n)
  (cond
    [(exact-nonnegative-integer? n)
     (unison-reference-builtin (string->chunked-string "Nat"))]
    [(exact-integer? n)
     (unison-reference-builtin (string->chunked-string "Int"))]
    [else
      (unison-reference-builtin (string->chunked-string "Float"))]))

(define (reflect-value v)
  (match v
    [(? exact-nonnegative-integer?)
     (unison-value-vlit (unison-vlit-pos v))]
    [(? exact-integer?)
     (unison-value-vlit (unison-vlit-neg (- v)))]
    [(? inexact-real?)
     (unison-value-vlit (unison-vlit-float v))]
    [(? char?)
     (unison-value-vlit (unison-vlit-char v))]
    [(? chunked-bytes?)
     (unison-value-vlit (unison-vlit-bytes v))]
    [(? bytes?)
     (unison-value-vlit (unison-vlit-bytearray v))]
    [(? vector?)
     (unison-value-vlit
       (unison-vlit-array
         (vector-map reflect-value v)))]
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
       (list->chunked-list (map reflect-value as)))]
    [(? procedure?)
     (unison-value-partial
       (function->groupref v)
       empty-chunked-list)]
    [(unison-data rf t fs)
     (unison-value-data
       (reflect-typelink rf)
       t
       (list->chunked-list (map reflect-value fs)))]))

(define (check-sandbox-ok ok l)
  (remove* ok (check-sandbox l)))

(define (sandbox-proc ok f)
  (check-sandbox-ok ok (lookup-function-link f)))

(define (sandbox-scheme-value ok v)
  (match v
    [(? chunked-list?)
     (for/fold ([acc '()]) ([e (in-chunked-list v)])
       (append (sandbox-value ok e) acc))]
    [(unison-closure f as)
     (for/fold ([acc (sandbox-proc ok f)]) ([a (in-list as)])
       (append (sandbox-scheme-value ok a) acc))]
    [(? procedure?) (sandbox-proc ok v)]
    [(unison-data rf t fs)
     (for/fold ([acc '()]) ([e (in-list fs)])
       (append (sandbox-scheme-value ok e) acc))]
    [else '()]))

(define (check-known l acc)
  (if (need-dependency? l) (cons l acc) acc))

; check sandboxing information for an internal.runtime.Value
(define (sandbox-value ok v)
  (for/fold
    ([sdbx '()]
     [unkn '()]

     #:result
     (if (null? unkn)
       (unison-either-right (list->chunked-list sdbx))
       (unison-either-left (list->chunked-list unkn))))

    ([r (in-chunked-list (value-term-dependencies v))])

    (let ([l (reference->termlink r)])
      (values
        (append (check-sandbox-ok ok l) sdbx)
        (check-known l unkn)))))

; check sandboxing information for a reflection.Value
(define (sandbox-quoted ok qv)
  (match qv
    [(unison-quote v) (sandbox-value ok v)]))

; replacment for Value.unsafeValue : a -> Value
(define-unison
  (builtin-Value.reflect v)
  (reflect-value v))

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

(define (gen-typelinks code)
  (map decode-syntax
       (chunked-list->list
         (gen-typelink-defns
           (list->chunked-list
             (map unison-code-rep code))))))

(define (gen-code args)
  (let-values ([(tl co) (splat-upair args)])
    (match tl
      [(unison-termlink-con r t)
       (raise "CACH: trying to add code for data constructor")]
      [(unison-termlink-builtin name)
       (raise "CACH: trying to add code for a builtin")]
      [(unison-termlink-derived bs i)
       (let* ([sg (unison-code-rep co)]
              [r (reflect-derived bs i)]
              [ds (cons
                    (gen-link-def r)
                    (chunked-list->list (gen-scheme r sg)))]
              [dc (decode-term (gen-link-decl r))])
         (append (map decode-syntax ds) (list dc)))])))

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

(define (register-code udefs)
  (for-each
    (lambda (p)
      (let-values ([(ln co) (splat-upair p)])
        (declare-code ln co)))
    udefs))

(define (add-module-associations links mname)
  (for-each
    (lambda (link)
      (let ([bs (termlink-bytes link)])
        (if (hash-has-key? runtime-module-map bs)
          #f
          (hash-set! runtime-module-map bs mname))))
    links))

(define (need-dependency? l)
  (let ([ln (if (unison-data? l) (reference->termlink l) l)])
    (and (unison-termlink-derived? ln) (not (have-code? ln)))))

(define (resolve-builtin nm)
  (dynamic-require
    'unison/primops
    nm
    (lambda ()
      (dynamic-require
        'unison/simple-wrappers
        nm))))

(define (termlink->proc tl)
  (match tl
    [(unison-termlink-derived bs i)
     (let ([mname (hash-ref runtime-module-map bs)])
       (parameterize ([current-namespace runtime-namespace])
         (dynamic-require `(quote ,mname) (termlink->name tl))))]
    [(unison-termlink-builtin name)
     (let ([mname (string->symbol (string-append "builtin-" name))])
       (parameterize ([current-namespace runtime-namespace])
         (resolve-builtin mname)))]))

(define (resolve-proc gr)
  (sum-case (decode-ref (group-reference gr))
    [0 (tx)
     (parameterize ([current-namespace runtime-namespace])
       (resolve-builtin
         (string->symbol (string-append "builtin-" tx))))]
    [1 (bs i)
     (let ([sym (group-ref-sym gr)]
           [mname (hash-ref runtime-module-map bs)])
       (parameterize ([current-namespace runtime-namespace])
         (dynamic-require `(quote ,mname) sym)))]))

; Straight-line module builder given intermediate definitions.
; This expects to receive a list of termlink, code pairs, and
; generates a scheme module that contains the corresponding
; definitions.
(define (build-intermediate-expressions primary dfns0)
  (let* ([udefs (chunked-list->list dfns0)]
         [pname (termlink->name primary)]
         [tmlinks (map ufst udefs)]
         [codes (map usnd udefs)]
         [tylinks (gen-typelinks codes)]
         [sdefs (flatten (map gen-code udefs))])
    `((require unison/boot
               unison/data-info
               unison/primops
               unison/primops-generated
               unison/builtin-generated
               unison/simple-wrappers
               unison/compound-wrappers)

      (provide main)

      ,@tylinks

      ,@sdefs

      (define (main)
        (handle ['ref-4n0fgs00] top-exn-handler
                (,pname #f))))))

(define (build-runtime-module mname tylinks tmlinks defs)
  (let ([names (map termlink->name tmlinks)])
    `(module ,mname racket/base
       (require unison/boot
                unison/data-info
                unison/primops
                unison/primops-generated
                unison/builtin-generated
                unison/simple-wrappers
                unison/compound-wrappers)

       (provide ,@names)

       ,@tylinks

       ,@defs)))

(define (add-runtime-module mname tylinks tmlinks defs)
  (eval (build-runtime-module mname tylinks tmlinks defs)
        runtime-namespace))

(define (code-dependencies co)
  (chunked-list->list
    (group-term-dependencies
      (unison-code-rep co))))

(define (add-runtime-code mname0 dfns0)
  (define (map-links dss)
    (map (lambda (ds) (map reference->termlink ds)) dss))

  (let ([udefs (chunked-list->list dfns0)])
    (cond
      [(not (null? udefs))
       (let* ([tmlinks (map ufst udefs)]
              [codes (map usnd udefs)]
              [refs (map termlink->reference tmlinks)]
              [depss (map code-dependencies codes)]
              [tylinks (gen-typelinks codes)]
              [deps (flatten depss)]
              [fdeps (filter need-dependency? deps)]
              [rdeps (remove* refs fdeps)])
         (cond
           [(null? fdeps) #f]
           [(null? rdeps)
            (let ([ndefs (map gen-code udefs)] [sdefs (flatten (map gen-code udefs))]
                  [mname (or mname0 (generate-module-name tmlinks))])
              (expand-sandbox tmlinks (map-links depss))
              (register-code udefs)
              (add-module-associations tmlinks mname)
              (add-runtime-module mname tylinks tmlinks sdefs)
              #f)]
           [else (list->chunked-list rdeps)]))]
      [else #f])))

(define (unison-POp-CACH dfns0)
  (let ([result (add-runtime-code #f dfns0)])
    (if result
      (sum 1 result)
      (sum 0 '()))))

(define (unison-POp-LOAD v0)
  (let* ([val (unison-quote-val v0)]
         [deps (value-term-dependencies val)]
         [fldeps (chunked-list->list deps)]
         [fdeps (filter need-dependency? (chunked-list->list deps))])
    (if (null? fdeps)
      (sum 1 (reify-value val))
        (sum 0 (list->chunked-list fdeps)))))

(define (unison-POp-LKUP tl) (lookup-code tl))

(define-unison (builtin-Code.lookup tl)
  (match (lookup-code tl)
    [(unison-sum 0 (list)) unison-optional-none]
    [(unison-sum 1 (list co)) (unison-optional-some co)]))

(define-unison (builtin-validateSandboxed ok v)
  (let ([l (sandbox-scheme-value (chunked-list->list ok) v)])
    (null? l)))

(define-unison (builtin-sandboxLinks tl) (check-sandbox tl))

(define-unison (builtin-Code.isMissing tl)
  (cond
    [(unison-termlink-builtin? tl) #f]
    [(unison-termlink-con? tl) #f]
    [(have-code? tl) #t]
    [else #f]))

(define-unison (builtin-Value.validateSandboxed ok v)
  (sandbox-quoted (chunked-list->list ok) v))
