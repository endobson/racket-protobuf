#lang racket/base


(provide
  (struct-out proto-identifiers)
  (struct-out message-identifiers)
  (struct-out singular-field-identifiers)
  (struct-out repeated-field-identifiers)
  (struct-out builder-identifiers)
  (struct-out builder-singular-field-identifiers)
  (struct-out builder-repeated-field-identifiers))

(struct proto-identifiers (message builder))

(struct message-identifiers (constructor fields parser serializer freezer) #:transparent)
(struct singular-field-identifiers (accessor) #:transparent)
(struct repeated-field-identifiers (accessor) #:transparent)

(struct builder-identifiers (constructor fields parser serializer copier) #:transparent)
(struct builder-singular-field-identifiers (accessor mutator available-predicate clearer) #:transparent)
(struct builder-repeated-field-identifiers
        (count accessor setter adder list-adder remover clearer
               index-builder-accessor list-builder-accessor builder-adder) #:transparent)
