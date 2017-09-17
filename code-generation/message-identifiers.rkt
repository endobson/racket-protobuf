#lang racket/base

(require
  "../message-identifiers.rkt"
  "../message-descriptor.rkt"
  racket/match
  racket/list
  racket/string
  racket/syntax)

(provide
  message-descriptor->proto-identifiers
  enum-descriptor->enum-identifiers)

(define (message-descriptor->proto-identifiers ctx md)
  (proto-identifiers
    (message-descriptor->message-identifiers ctx md)
    (message-descriptor->builder-identifiers ctx md)))

(define (camel-case->kebab-case s)
  (unless (regexp-match? #rx"([A-Z][a-z0-9]*)+" s)
    (error 'camel-case->kebab-case "Input is not CamelCase: ~s" s))
  (string-join
    (map
      (lambda (pair)
        (string-append
          (string-downcase (first pair))
          (second pair)))
      (regexp-match* #rx"([A-Z])([a-z0-9]*)" s #:match-select rest))
    "-"))


;; TODO make this do CamelCase and snake_case to hypen-case.
(define (message-descriptor->message-identifiers ctx md)
  (define name (message-descriptor-name md))
  (define kebab-name (camel-case->kebab-case (string-replace name #rx".*\\." "")))
  (message-identifiers
    (format-id ctx "make-~a" kebab-name)
    (for/hash ([(field-number fd) (message-descriptor-fields md)])
      (match-define (field-descriptor multiplicity type field-name) fd)
      (values
        field-number
        ((if (equal? 'optional multiplicity)
             singular-field-identifiers
             repeated-field-identifiers)
          (format-id ctx "~a-~a" kebab-name field-name))))
    (format-id ctx "parse-~a" kebab-name)
    (format-id ctx "write-~a" kebab-name)
    (format-id ctx "freeze-~a" kebab-name)))

;; TODO make this do CamelCase and snake_case to hypen-case.
(define (message-descriptor->builder-identifiers ctx md)
  (define name (message-descriptor-name md))
  (define short-name (string-replace name #rx".*\\." ""))
  (builder-identifiers
    (format-id ctx "~a-builder" short-name)
    (for/hash ([(field-number fd) (message-descriptor-fields md)])
      (match-define (field-descriptor multiplicity type field-name) fd)
      (define (format fmt . args)
        (format-id ctx fmt args))

      (values
        field-number
        ((case multiplicity
           [(optional) make-builder-singular-field-identifiers]
           [(repeated) make-builder-repeated-field-identifiers])
         ctx short-name field-name)))
    (format-id ctx "parse-~a-builder" short-name)
    (format-id ctx "write-~a-builder" short-name)
    #f))


(define (make-builder-singular-field-identifiers ctx name field)
  (builder-singular-field-identifiers
    (format-id ctx "~a-builder-~a" name field) ;; accessor
    (format-id ctx "set-~a-builder-~a!" name field) ;; mutator
    #f ;; available-predicate
    #f ;; clearer
    ))


(define (make-builder-repeated-field-identifiers ctx name field)
  (builder-repeated-field-identifiers
    #f ;; count
    (format-id ctx "~a-builder-~a" name field) ;; accessor
    #f ;; setter
    (format-id ctx "~a-builder-add-~a!" name field) ;; accessor
    #f ;; list-adder
    #f ;; remover
    #f ;; clearer
    #f ;; index-builder-accessor
    #f ;; list-builder-accessor
    #f ;; builder-adder
    ))


(define (enum-descriptor->enum-identifiers ctx descriptor)
  (define name (enum-descriptor-name descriptor))
  (enum-identifiers
    (format-id ctx "~a?" name)
    (format-id ctx "~a-list" name)
    (format-id ctx "~a->number" name)
    (format-id ctx "number->~a" name)))


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
;; foo-builder-add-b! : foo-builder? (or/c bar? bar-builder?) -> bar-builder?
;; foo-builder-add-b! : foo-builder? (or/c bar? bar-builder?) natural? -> bar-builder?
