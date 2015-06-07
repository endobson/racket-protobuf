#lang racket/base

(require "define-proto.rkt")
(provide (all-defined-out))

(define-proto
  (message "FileDescriptorSet"
    [repeated "FileDescriptorProto" file = 1])
  (message "FileDescriptorProto"
    [optional string name = 1]
    [optional string package = 2]
    [repeated string dependency = 3]
    [repeated int32 public_dependency = 10]
    [repeated int32 weak_dependency = 11]
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
    [repeated "FieldDescriptorProto" field = 2]
    #;
    [repeated "FieldDescriptorProto" extension = 6]
    [repeated "DescriptorProto" nested_type = 3]
    [repeated "EnumDescriptorProto" enum_type = 4]
    #;
    [repeated "ExtensionRange" extension_range = 5]
    #;
    [repeated "OneofDescriptorProto" oneof_decl = 8]
    #;
    [optional "MessageOptions" options = 7]
    [repeated "ReservedRange" reserved_range = 9]
    [optional string reserved_name = 10])
  (message "ReservedRange"
    [optional int32 start = 1]
    [optional int32 end = 2])
  (message "EnumDescriptorProto"
    [optional string name = 1]
    [repeated "EnumValueDescriptorProto" value = 2]
    #;
    [repeated "EnumOptions" options = 3])
  (message "EnumValueDescriptorProto"
    [optional string name = 1]
    [optional int32 number = 2]
    #;
    [optional "EnumValueOptions" options = 3])
  (message "FieldDescriptorProto"
    [optional string name = 1]
    [optional int32 number = 3]
    #;
    [optional "Label" label = 4]
    #;
    [optional "Type" type = 5]
    [optional string type_name = 6]
    [optional int32 oneof_index = 9]))
