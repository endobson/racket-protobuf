
(struct file-descriptor-set (file))

(struct file-descriptor
        (name package
         dependency public-dependency weak-dependency
         message-type enum-type service extension
         options
         source-code-info))

(struct descriptor-proto
        (name 
         field extension
         nested-type
         enum-type
         extension-range
         oneof-decl
         options))

(struct field-descriptor-proto
        (name 
         number
         label
         type
         type-name
         extendee
         default-value
         oneof-index
         options))


(define root-messages 
  (list
    (descriptor-proto
      "FileDescriptorSet"
      (list
        (field-descriptor-proto
          "file"
          "1"
          'LABEL_OPTIONAL
          'TYPE_MESSAGE
          "FileDescriptorProto"
          ""
          ""
          0
          #f))
      empty
      empty
      empty
      empty
      empty
      #f)))
(define root-enums empty)

(define root-file-descriptor
  (file-descriptor
    "descriptor.proto" "google.protobuf"
    '() '() '()
    root-messages
    root-enums
    '()
    '()
    #f
    #f))


(define (process-file-descriptor fd)
  (for/hash ([dp (file-descriptor-message-type fd)])
    (values
      (descriptor-proto-name dp)
      (for/hash ([field-dp (descriptor-proto-field dp)])
        (define multiplicity
          (case (field-descriptor-proto-label field-dp)
            [(LABEL_OPTIONAL) 'optional]
            [(LABEL_REPEATED) 'repeated]
            [else (error 'process-file-descriptor "Not all cases implemented")]))
        (define field-desc
          (case (field-descriptor-proto-type field-dp)
            [(TYPE_STRING) '(string)]
            [(TYPE_INT32) '(int32)]
            [(TYPE_BOOL) '(boolean)]
            [(TYPE_MESSAGE) (list 'message (field-descriptor-proto-type-name field-dp))]
            [else (error 'process-file-descriptor "Not all cases implemented")]))
        (values
          (field-descriptor-proto-number field-dp)
          (cons multiplicity field-desc))))))

(process-file-descriptor root-file-descriptor)



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
      8 '(optional message "OneOfDescriptorProto")
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
    (hash) ;TODO
    "FieldDescriptorProto"
    (hash) ;TODO
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

    ))


;(define pb (hex-string->bytes "120774657374696e67"))
(define pb (call-with-input-file* (expand-user-path "~/tmp/tmp.pb") port->bytes))

(define proto-port (open-input-bytes pb))
(define reader (scoped-read-proto descriptors))
(reader "FileDescriptorSet" proto-port)




;(extract-wire-key (read-proto-varint proto-port))
;(read-proto-varint proto-port)




