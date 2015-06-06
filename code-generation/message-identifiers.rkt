#lang racket/base

(require
  "../message-identifiers.rkt"
  "../message-descriptor.rkt"
  racket/match
  racket/syntax)

(provide message-descriptor->proto-identifiers)

(define (message-descriptor->proto-identifiers ctx md)
  (proto-identifiers
    (message-descriptor->message-identifiers ctx md)
    (message-descriptor->builder-identifiers ctx md)))


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

;; TODO make this do CamelCase and snake_case to hypen-case.
(define (message-descriptor->builder-identifiers ctx md)
  (define name (message-descriptor-name md))
  (builder-identifiers
    (format-id ctx "~a-builder" name)
    (for/hash ([(field-number fd) (message-descriptor-fields md)])
      (match-define (field-descriptor multiplicity type field-name) fd)
      (define (format fmt . args)
        (format-id ctx fmt args))

      (values
        field-number
        ((case multiplicity
           [(optional) make-builder-singular-field-identifiers]
           [(repeated) make-builder-repeated-field-identifiers])
         ctx name field-name)))
    (format-id ctx "parse-~a-builder" name)
    (format-id ctx "write-~a-builder" name)
    #f
    #f))


(define (make-builder-singular-field-identifiers ctx name field)
  (builder-singular-field-identifiers
    #f ;; accessor
    #f ;; mutator
    #f ;; available-predicate
    #f ;; clearer
    ))


(define (make-builder-repeated-field-identifiers ctx name field)
  (builder-repeated-field-identifiers
    #f ;; count
    #f ;; index-accessor
    #f ;; list-accessor
    #f ;; setter
    #f ;; adder
    #f ;; list-adder
    #f ;; remover
    #f ;; clearer
    #f ;; index-builder-accessor
    #f ;; list-builder-accessor
    #f ;; builder-adder
    ))

;; Names for:
;; message Foo {
;;  optional Bar a = 1;
;;  repeated Bar b = 2;
;; }
;;
;; make-foo : Keyworded function
;; make-foo-builder : Keyworded function
;;
;; foo-a ; foo? -> bar?
;; foo-b ; foo? -> (listof bar?)
;; foo-b ; foo? natural? -> bar?
;; foo-has-a? : foo? -> boolean?
;; foo-b-size : foo? -> natural?
;;
;; foo-builder-a ; foo-builder? -> bar?
;; foo-builder-a-builder ; foo-builder? -> bar-builder?
;; foo-builder-b ; foo-builder? -> (listof bar?)
;; foo-builder-b ; foo-builder? natural -> bar?
;; foo-builder-b-builder ; foo-builder? -> bar-builder?
;; foo-builder-b-builder ; foo-builder? natural? -> bar-builder?
;; set-foo-builder-a! : foo-builder? (or/c bar? bar-builder?) -> void?
;; set-foo-builder-b! : foo-builder? natural? (or/c bar? bar-builder?) -> void?
