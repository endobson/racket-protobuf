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
                    'field-type)) ...)))]))
  (define mds2
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
        [optional int32 oneof_index = 9])

      
      ))
  (define mds3
    (for/hash ([md (in-list mds2)])
      (values (message-descriptor-name md) md)))

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


  (define mds4
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
                    (second field)))))))))
  (define mds mds3))


(define-syntax (go stx)
  (define mids
    (for/hash ([(name md) (in-hash mds)])
      (values name
        (message-identifiers
          (format-id stx "~a" name)
          (for/hash ([(field-number fd) (message-descriptor-fields md)])
            (match-define (field-descriptor multiplicity type) fd)
            (values
              field-number
              ((if (equal? 'optional multiplicity)
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

(pretty-print
  (FileDescriptorSet-parser proto-port (FileDescriptorSet)))
(pretty-print
  (FileDescriptorSet-parser proto-port2 (FileDescriptorSet)))




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
