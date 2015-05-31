#lang racket/base

(require
  "proto-reader.rkt"
  "define-proto.rkt"
  racket/pretty
  racket/port
  racket/runtime-path)

(define-runtime-path tmp-pb-path "tests/test-data/tmp.pb")
(define-runtime-path descriptor-pb-path "tests/test-data/descriptor.pb")

(define pb (call-with-input-file* tmp-pb-path port->bytes))
(define pb2 (call-with-input-file* descriptor-pb-path port->bytes))

(define proto-port (open-input-bytes pb))
(define proto-port2 (open-input-bytes pb2))


(define-proto
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
    [optional int32 oneof_index = 9]))




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
