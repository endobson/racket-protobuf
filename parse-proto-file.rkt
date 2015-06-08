#lang racket/base

(require
  racket/match
  racket/port
  racket/promise
  racket/system
  "proto-descriptors.rkt")

(provide parse-proto-file)

(define (parse-proto-file path)
  (define-values (base-dir filename dir?) (split-path path))
  (match (process* "/usr/bin/env" "protoc" path "-o" "/dev/stdout" "-I" base-dir)
    [(list stdout stdin pid stderr handler)
     (define parsed-output
       (delay/thread
         (dynamic-wind
           void
           (lambda ()
             (parse-FileDescriptorSet stdout))
           (lambda ()
             (close-input-port stdout)))))
     (define collected-stderr
       (delay/thread
         (dynamic-wind
           void
           (lambda ()
             (port->string stderr))
           (lambda ()
             (close-input-port stderr)))))
     (close-output-port stdin)
     (handler 'wait)
     (define exit-code (handler 'exit-code))
     (cond
       [(zero? exit-code)
        (force collected-stderr)
        (force parsed-output)]
       [else
         (force parsed-output)
         (error 'parse-proto-file
                "Error executing protoc; exit code ~a~n~n~a"
                exit-code
                (force collected-stderr))])]))

