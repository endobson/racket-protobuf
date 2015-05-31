#lang racket/base

(require
  "../message-identifiers.rkt"
  "../message-descriptor.rkt"
  racket/match
  racket/syntax)

(provide message-descriptor->message-identifiers)

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

