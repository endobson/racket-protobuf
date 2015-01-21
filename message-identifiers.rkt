#lang racket/base


(provide
  (struct-out message-identifiers)
  (struct-out singular-field-identifiers)
  (struct-out repeated-field-identifiers))

(struct message-identifiers (constructor fields parser serializer) #:transparent)
(struct singular-field-identifiers (accessor mutator) #:transparent)
(struct repeated-field-identifiers (accessor adder) #:transparent)
