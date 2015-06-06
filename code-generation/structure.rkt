#lang racket/base

(require
  (for-template
    racket/base
    racket/list)

  "../message-identifiers.rkt"
  "../message-descriptor.rkt"
  racket/match
  racket/syntax)

(provide
  generate-message-structure
  generate-builder-structure)


(define (generate-message-structure message-ids desc)
  (match-define (message-descriptor name fields) desc)
  (define ids (proto-identifiers-message (hash-ref message-ids name)))
  (define/with-syntax constructor (message-identifiers-constructor ids))
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


  (define num-fields (hash-count fields))
  #`(begin
      (define-values (type-descriptor raw-constructor predicate accessor mutator)
        (make-struct-type '#,(string->symbol name) #f #,num-fields 0 #f empty #f))
      (define (constructor)
        (raw-constructor
          #,@(for/list ([i num-fields])
               (case (field-descriptor-multiplicity (hash-ref fields (hash-ref reverse-indices i)))
                 [(optional) #'#f]
                 [(repeated) #'null]))))
      ;; Make deterministic
      #,@(for/list ([(field-number field-ids) (message-identifiers-fields ids)])
           (define field-index (hash-ref indices field-number))
           (match field-ids
             [(singular-field-identifiers acc mut)
              #`(begin
                  (define #,acc
                    (make-struct-field-accessor accessor #,field-index))
                  (define #,mut
                    (make-struct-field-mutator mutator #,field-index)))]
             [(repeated-field-identifiers acc adder)
              (define mut (generate-temporary 'mut))
              #`(begin
                  (define #,acc
                    (make-struct-field-accessor accessor #,field-index))
                  (define #,mut
                    (make-struct-field-mutator mutator #,field-index))
                  (define (#,adder arg new)
                    (#,mut arg (cons new (#,acc arg)))))]))))

(define (generate-builder-structure message-ids desc)
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
        (make-struct-type '#,(string->symbol name) #f #,num-fields 0 #f empty #f))
      (define (constructor)
        (raw-constructor
          #,@(for/list ([i num-fields])
               (case (field-descriptor-multiplicity (hash-ref fields (hash-ref reverse-indices i)))
                 [(optional) #'#f]
                 [(repeated) #'null]))))))
