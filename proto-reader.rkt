#lang racket/base
(require
  racket/port
  racket/match
  racket/list)

(provide (all-defined-out))

(define (read-proto-varint port [allow-eof? #f])
  (cond
    [(eof-object? (peek-byte port))
     (if allow-eof?
         eof
         (error 'parse-proto "Bytestream ended while parsing a varint"))]
    [else
      (let loop ([acc 0] [mult 1])
        (define byte (read-byte port))
        (define val (bitwise-bit-field byte 0 7))
        (when (eof-object? byte)
          (error 'parse-proto "Bytestream ended while parsing a varint"))
        (define cont (bitwise-bit-set? byte 7))
        (define new-acc (+ acc (* mult val)))
        (if cont
            (loop new-acc (* mult 128))
            new-acc))]))

(define (read-proto-string port length)
  (define bytes (read-bytes length port))
  (unless (equal? (bytes-length bytes) length)
    (error 'parse-proto "Bytestream ended in the middle of a string field"))
  (bytes->string/utf-8 bytes))

(define (read-proto-bytes port length)
  (define bytes (read-bytes length port))
  (unless (equal? (bytes-length bytes) length)
    (error 'parse-proto "Bytestream ended in the middle of a bytes field"))
  bytes)


(define (extract-wire-key value)
  (define encoding
    (case (bitwise-bit-field value 0 3)
      [(0) 'varint]
      [(1) '64bit]
      [(2) 'length-delimited]
      [(5) '32bit]
      [else (error 'extract-wire-key "Unknown encoding")]))
  (values (arithmetic-shift value -3)
          encoding))

(define (read-proto-primitive port type)
  (case type
    [(string) (read-proto-string port (read-proto-varint port))]
    [(bytes) (read-proto-bytes port (read-proto-varint port))]
    [(int32) (read-proto-varint port)]
    [else (error 'read-proto-primitive "Not yet implemented: ~a" type)]))

(define (drop-proto port wire-type)
  (case wire-type
    [(varint) (read-proto-varint port)]
    [(length-delimited)
     ;; TODO just throw away the results
     (read-proto-bytes port (read-proto-varint port))]
    [else
     (error 'parse-proto "Not yet implemented: ~a" wire-type)]))


(define (scoped-read-proto descriptors)
  (define (recur type port)
    (define descriptor (hash-ref descriptors type))

    (define (add-field multiplicity number value acc)
      (case multiplicity
        [(optional) (hash-set acc number value)] ;; TODO do a merge
        [(repeated) (hash-update acc number (λ (vs) (cons value vs)) empty)]
        [else (error 'scoped-read-proto "Bad multiplicity")]))

    (define (handle-field number kind acc)
      (match (hash-ref descriptor number #f)
        [(list multiplicity 'string)
         (unless (equal? kind 'length-delimited)
           (error 'scoped-read-proto "Bad field"))
         (add-field multiplicity number (read-proto-string port (read-proto-varint port)) acc)]
        [(list multiplicity 'bytes)
         (unless (equal? kind 'length-delimited)
           (error 'scoped-read-proto "Bad field"))
         (add-field multiplicity number (read-proto-bytes port (read-proto-varint port)) acc)]
        [(list multiplicity 'message sub-message-name)
         (unless (equal? kind 'length-delimited)
           (error 'scoped-read-proto "Bad field"))
         (define sub-value
           (recur sub-message-name (make-limited-input-port port (read-proto-varint port))))
         (add-field multiplicity number sub-value acc)]
        [(list multiplicity 'int32)
         (unless (equal? kind 'varint)
           (error 'scoped-read-proto "Bad field"))
         (add-field multiplicity number (read-proto-varint port) acc)]
        [#f
         (case kind
           [(varint) (read-proto-varint port)]
           [(length-delimited)
            (read-proto-bytes port (read-proto-varint port))]
           [else
             (error 'scoped-read-proto "Not yet implemented: ~a" type)])
         acc]))

    (define (handle-acc acc)
      (cons type (hash->list acc)))
    (define (field-loop acc)
      (define varint (read-proto-varint port #t))
      (if (eof-object? varint)
          (handle-acc acc)
          (let-values ([(field-number kind) (extract-wire-key varint)])
            (field-loop (handle-field field-number kind acc)))))
    (field-loop (hash)))
  recur)


