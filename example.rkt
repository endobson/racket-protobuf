#lang racket/base

(require
  (for-syntax
    racket/base
    racket/syntax
    racket/list
    "code-generation.rkt")
  "proto-reader.rkt"
  racket/port
  )


(define pb (call-with-input-file* (expand-user-path "~/tmp/tmp.pb") port->bytes))

(define proto-port (open-input-bytes pb))


(begin-for-syntax
  (define descriptors
    (hash
      "FileDescriptorSet"
      (hash
        1 '(repeated message "FileDescriptorProto"))
      "FileDescriptorProto"
      (hash
        1 '(optional string)
        2 '(optional string)
        3 '(repeated string)
        10 '(repeated int32)
        11 '(repeated int32)
        4 '(repeated message "DescriptorProto")
        5 '(repeated message "EnumDescriptorProto")
        6 '(repeated message "ServiceDescriptorProto")
        7 '(repeated message "FieldDescriptorProto")
        8 '(optional message "FileOptions")
        9 '(optional message "SourceCodeInfo"))
      "DescriptorProto"
      (hash
        1 '(optional string)
        2 '(optional message "FieldDescriptorProto")
        6 '(optional message "FieldDescriptorProto")
        3 '(optional message "DescriptorProto")
        4 '(optional message "EnumDescriptorProto")
        5 '(optional message "ExtensionRange")
        8 '(optional message "OneofDescriptorProto")
        7 '(optional message "MessageOptions"))
      "EnumDescriptorProto"
      (hash
        1 '(optional string)
        2 '(repeated message "EnumValueDescriptorProto")
        3 '(optional message "EnumOptions"))
      "EnumValueDescriptorProto"
      (hash
        1 '(optional string)
        2 '(optional int32)
        3 '(optional message "EnumValueOptions"))
      "EnumOptions"
      (hash
        2 '(optional boolean)
        3 '(optional boolean))
      "EnumValueOptions"
      (hash
        1 '(optional boolean))
  
  
      "ServiceDescriptorProto"
      (hash
        1 '(optional string)
        2 '(repeated message "MethodDescriptorProto")
        3 '(optional message "ServiceOptions"))
  
      "FieldDescriptorProto"
      (hash
        1 '(optional string)
        3 '(optional int32)
        ;4 TODO
        ;5 TODO
        6 '(optional string)
        2 '(optional string)
        7 '(optional string)
        9 '(optional int32)
        8 '(optional message "FieldOptions"))
  
      "FileOptions"
      (hash) ;TODO
      "SourceCodeInfo"
      (hash) ;TODO
      "ExtensionRange"
      (hash) ;TODO
      "OneofDescriptorProto"
      (hash) ;TODO
      "MessageOptions"
      (hash) ;TODO
      "MethodDescriptorProto"
      (hash) ;TODO
      "ServiceOptions"
      (hash) ;TODO
      "FieldOptions"
      (hash) ;TODO
  
      ))


  (define mds
    (for/hash ([(name fields) (in-hash descriptors)])
      (values name
        (message-descriptor 
          name
          (for/hash ([(field-number field) fields])
            (values
              field-number
              (field-descriptor
                (first field)
                (if (equal? 'message (second field))
                    (third field)
                    (second field))))))))))


(define-syntax (go stx)
  (define mids
    (for/hash ([(name fields) (in-hash descriptors)])
      (values name
        (message-identifiers
          (format-id stx "~a" name)
          (for/hash ([(field-number field) fields])
            (values
              field-number
              ((if (equal? 'optional (first field))
                   singular-field-identifiers
                   repeated-field-identifiers)
                (format-id stx "~a-~a" name field-number)
                (format-id stx "set-~a-~a!" name field-number))))
          (format-id stx "~a-parser" name)
          (format-id stx "~a-writer" name)))))

  #`(begin
      #,@(for/list ([(name md) mds])
           #`(begin
               #,(generate-structure mids md)
               #,(generate-parser mids md)))))

(go)

(FileDescriptorSet-parser proto-port (FileDescriptorSet))

;(for ([(name fields) (in-hash descriptors)])
;  (set-message-descriptor-fields!
;    (hash-ref mds name)
;    (for/hash ([(field-number field) (in-hash fields)])
;      (values
;        field-number
;        (field-descriptor
;          (first field)
;          (cond
;           [(equal? 'message (second field))
;            (hash-ref mds (third field))]
;           [else (second field)]))))))



;(parse-proto proto-port (hash-ref mds "FileDescriptorSet"))
