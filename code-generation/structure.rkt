#lang racket/base

(require
  (for-template
    racket/function
    racket/base
    racket/list
    "template-code.rkt"
    "../varint.rkt")

  "../message-identifiers.rkt"
  "../message-descriptor.rkt"
  "../varint.rkt"
  racket/list
  racket/match
  racket/function
  racket/syntax)

(provide
  generate-message-structure
  generate-builder-structure)


(define (generate-message-structure message-ids enum-ids desc)
  (match-define (message-descriptor name fields) desc)
  (define ids (proto-identifiers-message (hash-ref message-ids name)))
  (define builder-ids (proto-identifiers-builder (hash-ref message-ids name)))
  (define/with-syntax constructor (message-identifiers-constructor ids))
  (define/with-syntax size-constructor (generate-temporary (message-identifiers-constructor ids)))
  (define/with-syntax freezer (message-identifiers-freezer ids))
  (define/with-syntax (type-descriptor raw-constructor predicate accessor mutator)
    (generate-temporaries '(type-descriptor raw-constructor predicate accessor mutator)))

  ;; Hash of field-number to index of the field in the struct.
  (define indices
    (for/hash ([field-index (in-naturals)]
               [(field-number field-ids) (message-identifiers-fields ids)])
      (values field-number field-index)))

  ;; Hash of index of the field in the struct to field number.
  (define reverse-indices
    (for/hash ([(k v) (in-hash indices)])
      (values v k)))


  ;; This is the number of proto fields
  (define num-fields (hash-count fields))
  ;; The super struct 'protobuf-base' has the following fields:
  ;; * byte-size: the size of the serialized proto in bytes.
  #`(begin
      (define-values (type-descriptor raw-constructor predicate accessor mutator)
        (make-struct-type '#,(string->symbol name) struct:protobuf-base #,num-fields 0 #f empty #f #f
                          '#,(build-list num-fields identity)))

      #,(let ([args (generate-temporaries (build-list num-fields identity))])
          #`(define (size-constructor #,@args)
              (let ([size 0])
                #,(for/fold ([body #`(raw-constructor size #,@args)])
                             ([i (in-range num-fields)]
                              [arg (in-list args)])
                     (define field-num (hash-ref reverse-indices i))
                     (define fd (hash-ref fields field-num))
                     (define new-size-stx
                       (case (field-descriptor-multiplicity fd)
                         [(optional)
                          #`(if (equal? 
                                  #,arg
                                  #,(default-value enum-ids (field-descriptor-type fd)))
                                0
                                (+ #,(tag-size field-num) 
                                   #,(value-size arg enum-ids (field-descriptor-type fd))))]
                         [(repeated)
                          #`(for/sum ([sub (in-list #,arg)])
                              (+ #,(tag-size field-num) 
                                 #,(value-size #`sub enum-ids (field-descriptor-type fd))))]))
                    #`(let ([size (+ size #,new-size-stx)]) #,body)))))

      #,(let ([args (generate-temporaries (build-list num-fields identity))])
          (define keyword-arguments
            (for/list ([i (in-range num-fields)]
                       [arg (in-list args)])
              (define field-num (hash-ref reverse-indices i))
              (define fd (hash-ref fields field-num))
              (list
                (string->keyword (field-descriptor-name fd))
                #`[#,arg
                   #,(case (field-descriptor-multiplicity fd)
                       [(optional) (default-value enum-ids (field-descriptor-type fd))]
                       [(repeated) #'null])])))

          #`(define (constructor #,@(append* keyword-arguments))
              (size-constructor #,@args)))

      (define (freezer builder)
        (size-constructor
          #,@(for/list ([i num-fields])
               (define fd (hash-ref fields (hash-ref reverse-indices i)))
               (define builder-field-identifiers (hash-ref (builder-identifiers-fields builder-ids)
                                                           (hash-ref reverse-indices i)))
               (define/with-syntax freeze
                 (if (message-type? (field-descriptor-type fd))
                     (with-syntax ([freezer (message-identifiers-freezer
                                              (proto-identifiers-message
                                                (hash-ref message-ids (second (field-descriptor-type fd)))))])
                       #'(λ (b) (and b (freezer b))))
                     #'identity))
               (define/with-syntax lift
                 (if (eq? 'repeated (field-descriptor-multiplicity fd))
                     #'(λ (f) (λ (l) (map f l)))
                     #'identity))
               (define/with-syntax field-acc
                 (if (eq? 'repeated (field-descriptor-multiplicity fd))
                     (builder-repeated-field-identifiers-accessor builder-field-identifiers)
                     (builder-singular-field-identifiers-accessor builder-field-identifiers)))
               #'((lift freeze) (field-acc builder)))))

      ;; TODO(endobson) Make deterministic
      #,@(for/list ([(field-number field-ids) (message-identifiers-fields ids)])
           (define/with-syntax field-index (hash-ref indices field-number))
           (define/with-syntax field-name
             (string->symbol (field-descriptor-name (hash-ref fields field-number))))
           (match field-ids
             [(singular-field-identifiers acc)
              #`(define #,acc
                  (make-struct-field-accessor accessor field-index 'field-name))]
             [(repeated-field-identifiers acc)
              #`(define #,acc
                  (make-struct-field-accessor accessor field-index 'field-name))]))))

(define (generate-builder-structure message-ids enum-ids desc)
  (match-define (message-descriptor name fields) desc)
  (define ids (proto-identifiers-builder (hash-ref message-ids name)))
  (define/with-syntax constructor (builder-identifiers-constructor ids))
  (define/with-syntax (type-descriptor raw-constructor predicate accessor mutator)
    (generate-temporaries '(type-descriptor raw-constructor predicate accessor mutator)))

  ;; Hash of field-number to index of the field in the struct.
  (define indices
    (for/hash ([field-index (in-naturals)]
               [(field-number field-ids) (builder-identifiers-fields ids)])
      (values field-number field-index)))

  ;; Hash of index of the field in the struct to field number.
  (define reverse-indices
    (for/hash ([(k v) (in-hash indices)])
      (values v k)))


  (define num-fields (hash-count fields))
  #`(begin
      (define-values (type-descriptor raw-constructor predicate accessor mutator)
        (make-struct-type '#,(string->symbol (string-append name "-builder")) #f #,num-fields 0 #f empty #f))
      (define (constructor)
        (raw-constructor
          #,@(for/list ([i num-fields])
               (define fd (hash-ref fields (hash-ref reverse-indices i)))
               (case (field-descriptor-multiplicity fd)
                 [(optional) (default-value enum-ids (field-descriptor-type fd))]
                 [(repeated) #'null]))))

      ;; TODO(endobson) Make deterministic
      #,@(for/list ([(field-number field-ids) (builder-identifiers-fields ids)])
           (define/with-syntax field-index (hash-ref indices field-number))
           (define/with-syntax field-name
             (string->symbol (field-descriptor-name (hash-ref fields field-number))))
           (match field-ids
             [(builder-singular-field-identifiers acc mut available-predicate clearer)
              #`(begin
                  (define #,acc
                    (make-struct-field-accessor accessor field-index 'field-name))
                  (define #,mut
                    (make-struct-field-mutator mutator field-index 'field-name)))]
             [(builder-repeated-field-identifiers
               count acc setter adder list-adder remover clearer
               index-builder-accessor list-builder-accessor builder-adder)
              (define mut (generate-temporary 'mut))
              #`(begin
                  (define #,acc
                    (make-struct-field-accessor accessor field-index 'field-name))
                  (define #,mut
                    (make-struct-field-mutator mutator field-index 'field-name))
                  (define (#,adder builder new)
                    (#,mut builder (append (#,acc builder) (list new)))))]))))

;; The default value for a given proto buf type.
(define (default-value enum-ids type)
  (match type
    ['int32 #'0]
    ['string #'""]
    ['bytes #'#""]
    ['boolean #'#f]
    [(list 'message (? string?)) #'#f]
    [(list 'enum (? string? enum-type))
     #`(#,(enum-identifiers-number->enum (hash-ref enum-ids enum-type)) 0)]))

(define (message-type? type)
  (match type
    [(list 'message _) #t]
    [_ #f]))

(define (tag-size tag)
  (varint-size (arithmetic-shift tag 3)))

(define (value-size arg-stx enum-ids type)
  (match type
    ['int32 #`(varint-size #,arg-stx)]
    ['string #`(let ([len (string-utf-8-length #,arg-stx)])
                 (+ (varint-size len) len))]
    ['bytes #`(let ([len (bytes-length #,arg-stx)])
                 (+ (varint-size len) len))]
    ['boolean 1]
    [(list 'message (? string? subtype))
     #`(let ([len (protobuf-base-byte-size #,arg-stx)])
         (+ (varint-size len) len))]
    [(list 'enum (? string? enum-type))
     #`(varint-size (#,(enum-identifiers-enum->number (hash-ref enum-ids enum-type)) #,arg-stx))]))

