#lang racket/base

(require racket/contract)

(provide
  (contract-out
    [runtime-message-descriptor
      (->i #:chaperone
        ([predicate predicate/c]
         [parser (predicate) (-> port? predicate)]
         [serializer (predicate) (-> predicate port? void)])
        [_ runtime-message-descriptor?])]
    [runtime-message-descriptor? predicate/c]
    [runtime-message-descriptor-predicate (-> runtime-message-descriptor? any/c)]
    [runtime-message-descriptor-parser (-> runtime-message-descriptor? any/c)]
    [runtime-message-descriptor-serializer (-> runtime-message-descriptor? any/c)]))

(struct runtime-message-descriptor
  (predicate
   parser
   serializer))


