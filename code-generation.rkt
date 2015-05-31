#lang racket/base

(require
  (for-template
    racket/base
    racket/port
    racket/list
    "proto-reader.rkt")

  "message-identifiers.rkt"
  "message-descriptor.rkt"
  racket/match
  racket/syntax)

(provide generate-code)

;; A protocol message has the following things at runtime
;; Field accessors

;; Constructor


;; This generates the code given a list of messages.
;; ctx: syntax? The lexical context for generated identifiers.
;; mds: (listof message-descriptor?) The messages to generate code for.
(define (generate-code ctx mds)
  (define mids
    (for/hash ([md (in-list mds)])
      (values
        (message-descriptor-name md)
        (message-descriptor->message-identifiers ctx md))))

  #`(begin
      #,@(for/list ([md (in-list mds)])
           #`(begin
               #,(generate-structure mids md)
               #,(generate-parser mids md)))))




(define (type->expected-wire-type type)
  (match type
    [(or 'string 'bytes (? string?))
     'length-delimited]
    [(or 'int32 'boolean)
     'varint]))

(define (generate-structure message-ids desc)
  (match-define (message-descriptor name fields) desc)
  (define ids (hash-ref message-ids name))
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

;; TODO make this do CamelCase and snake_case to hypen-case.
(define (message-descriptor->message-identifiers ctx md)
  (define name (message-descriptor-name md))
  (message-identifiers
    (format-id ctx "~a" name)
    (for/hash ([(field-number fd) (message-descriptor-fields md)])
      (match-define (field-descriptor multiplicity type field-name) fd)
      (values
        field-number
        ((if (equal? 'optional multiplicity)
             singular-field-identifiers
             repeated-field-identifiers)
          (format-id ctx "~a-~a" name field-name)
          (format-id ctx "set-~a-~a!" name field-name))))
    (format-id ctx "parse-~a" name)
    (format-id ctx "write-~a" name)))




(define (generate-parser message-ids desc)
  (define name  (message-descriptor-name desc))
  (define ids (hash-ref message-ids name))
  (define parser (message-identifiers-parser ids))
  (define field-clauses
    (for/list ([(field-number fd) (message-descriptor-fields desc)])
      (match-define (field-descriptor multiplicity type _) fd)
      #`[(#,field-number)
         (unless (equal? wire-type '#,(type->expected-wire-type type))
           (error 'parse-proto "Bad wire type"))
         #,(case multiplicity
             [(optional)
              (match-define (singular-field-identifiers accessor* mutator*)
                (hash-ref (message-identifiers-fields ids) field-number))
              (define/with-syntax accessor accessor*)
              (define/with-syntax mutator mutator*)
              (case type
               [(string)
                #`(let ([amount (read-proto-varint port)])
                    (mutator current-proto (read-proto-string port amount)))]
               [(bytes) #`(mutator current-proto
                           (read-proto-bytes port (read-proto-varint port)))]
               ;; TODO make this cap in 32 bit range
               [(int32) #`(mutator current-proto (read-proto-varint port))]
               ;; TODO make this work
               [(boolean) #`(error 'nyi)]
               [else
                 (define sub-ids (hash-ref message-ids type))
                 (define/with-syntax sub-constructor
                   (message-identifiers-constructor sub-ids))
                 (define/with-syntax sub-parser
                   (message-identifiers-parser sub-ids))
                 #'(let ()
                     (define sub-message
                       (let ([old-value (accessor current-proto)])
                         (or old-value
                             (let ([new-value (sub-constructor)])
                               (mutator current-proto new-value)
                               new-value))))
                     (define len (read-proto-varint port))
                     (sub-parser
                       (make-limited-input-port port len)
                       sub-message))])]
             [(repeated)
              (match-define (repeated-field-identifiers accessor* adder*)
                (hash-ref (message-identifiers-fields ids) field-number))
              (define/with-syntax accessor accessor*)
              (define/with-syntax adder adder*)
              #`(adder current-proto
                  #,(case type
                     [(string) #'(read-proto-string port (read-proto-varint port))]
                     [(bytes) #'(read-proto-bytes port (read-proto-varint port))]
                     ;; TODO make this cap in 32 bit range
                     [(int32) #'(read-proto-varint port)]
                     ;; TODO make this work
                     [(boolean) #`(error 'nyi)]
                     [else
                       (define sub-ids (hash-ref message-ids type))
                       (define/with-syntax sub-constructor
                         (message-identifiers-constructor sub-ids))
                       (define/with-syntax sub-parser
                         (message-identifiers-parser sub-ids))
                       #'(let ([len (read-proto-varint port)])
                           (sub-parser
                             (make-limited-input-port port len)
                             (sub-constructor)))]))])]))


  #`(define (#,parser port current-proto)
      (let loop ()
        (define varint (read-proto-varint port #t))
        (unless (eof-object? varint)
          (define-values (field-number wire-type) (extract-wire-key varint))
          (case field-number
            #,@field-clauses
            [else
              ;; Read through the unknown field
              (case wire-type
                [(varint) (read-proto-varint port)]
                [(length-delimited)
                 (define len (read-proto-varint port))
                 (read-proto-bytes port len)]
                [else (error 'nyi)])])
          (loop)))
      current-proto))
