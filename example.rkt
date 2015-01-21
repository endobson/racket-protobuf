#lang racket/base

(require
  (for-syntax
    racket/base
    racket/syntax
    racket/list
    racket/pretty
    racket/match
    
    (for-syntax
      syntax/parse
      syntax/parse/experimental/template
      racket/base)
    "message-identifiers.rkt"
    "message-descriptor.rkt"
    "code-generation.rkt")
  "proto-reader.rkt"
  racket/pretty
  racket/port
  )


(define pb (call-with-input-file* (expand-user-path "~/tmp/tmp.pb") port->bytes))
(define pb2 (call-with-input-file* (expand-user-path "~/tmp/descriptor.pb") port->bytes))

(define proto-port (open-input-bytes pb))
(define proto-port2 (open-input-bytes pb2))

(begin-for-syntax
  (begin-for-syntax
    (define-syntax-class proto-type
      (pattern (~datum string))
      (pattern (~datum boolean))
      (pattern (~datum int32))
      (pattern type-ref:str))))




(begin-for-syntax
  (define-syntax message
    (syntax-parser 
      [(_ name:str
          ((~and field-multiplicity (~or (~datum optional) (~datum repeated)))
           field-type:proto-type
           field-name:id
           (~datum =)
           field-number:exact-positive-integer
           ) ...)
        (template
          (message-descriptor 
            name
            (hash
              (?@ field-number
                  (field-descriptor
                    'field-multiplicity
                    'field-type
                    'field-name)) ...)))]))
  (define md-list
    (list
      (message "FileDescriptorSet"
        [repeated "FileDescriptorProto" file = 1])
      (message "FileDescriptorProto"
        [optional string name = 1]
        [optional string package = 2]
        [repeated string dependency = 3]
        [repeated int32 public_dependency = 10]
        [repeated int32 weak_dpendency = 11]
        [repeated "DescriptorProto" message_type = 4]
        [repeated "EnumDescriptorProto" enum_type = 5]
        #;
        [repeated "ServiceDescriptorProto" service = 6]
        #;
        [repeated "FieldDescriptorProto" extension = 7]
        #;
        [optional "FileOptions" options = 8]
        #;
        [optional "SourceCodeInfo" source_code_info = 9])
      (message "DescriptorProto"
        [optional string name = 1]
        #;
        [repeated "FieldDescriptorProto" field = 2]
        #;
        [repeated "FieldDescriptorProto" extension = 6]
        [repeated "DescriptorProto" nested_type = 3]
        #;
        [repeated "EnumDescriptorProto" enum_type = 4]
        #;
        [repeated "ExtensionRange" extension_range = 5]
        #;
        [repeated "OneofDescriptorProto" oneof_decl = 8]
        #;
        [optional "MessageOptions" options = 7])
      (message "EnumDescriptorProto"
        [optional string name = 1]
        #;
        [repeated "EnumValueDescriptorProto" value = 2]
        #;
        [repeated "EnumOptions" options = 3])
      (message "FieldDescriptorProto"
        [optional string name = 1]
        [optional int32 number = 3]
        #;
        [optional "Label" label = 4]
        #;
        [optional "Type" type = 5]
        [optional string type_name = 6]
        [optional int32 oneof_index = 9])))

  (define mds
    (for/hash ([md (in-list md-list)])
      (values (message-descriptor-name md) md))))


(define-syntax (go stx)
  (define mids
    (for/hash ([(name md) (in-hash mds)])
      (values name
        (message-identifiers
          (format-id stx "~a" name)
          (for/hash ([(field-number fd) (message-descriptor-fields md)])
            (match-define (field-descriptor multiplicity type field-name) fd)
            (values
              field-number
              ((if (equal? 'optional multiplicity)
                   singular-field-identifiers
                   repeated-field-identifiers)
                (format-id stx "~a-~a" name field-name)
                (format-id stx "set-~a-~a!" name field-name))))
          (format-id stx "parse-~a" name)
          (format-id stx "write-~a" name)))))

  #`(begin
      #,@(for/list ([(name md) mds])
           #`(begin
               #,(generate-structure mids md)
               #,(generate-parser mids md)))))

(go)

(pretty-print
  (parse-FileDescriptorSet proto-port (FileDescriptorSet)))
(pretty-print
  (parse-FileDescriptorSet proto-port2 (FileDescriptorSet)))




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
