#lang racket/base

(module* main #f
  (require
    compiler/compiler
    racket/cmdline
    racket/file
    protobuf/proto-descriptors)

  (define links-arg #f)
  (define file-descriptor-arg #f)
  (define bin-dir-arg #f)
  (define output-dir-arg #f)
  
  (command-line
    #:once-each 
    [("--links") links "Link files"
     (set! links-arg links)]
    [("--file_descriptor") file-descriptor "File descriptor proto"
     (set! file-descriptor-arg file-descriptor)]
    [("--bin_dir") directory "Bin directory"
     (set! bin-dir-arg directory)]
    [("--output_dir") directory "Output directory"
     (set! output-dir-arg directory)])
  
  ;; Setup collection-links
  (define cwd (current-directory))
  (define links
    (read (open-input-string links-arg)))
  (current-library-collection-links 
    (cons #f (map (lambda (p) (build-path cwd p)) links)))
  
  (define compile (compile-zos #f #:module? #t))
  (define count 0)

  (define file-descriptor-set
    (call-with-input-file file-descriptor-arg
      (lambda (port)
        (parse-file-descriptor-set port))))
  (for ([file (in-list (file-descriptor-set-file file-descriptor-set))])
    (define full-proto-file-name (file-descriptor-proto-name file))
    (define-values (dir-name proto-file-name must-dir?) (split-path full-proto-file-name))
    (define source-path
      (regexp-replace
        #rx".proto$"
        (path->string (build-path bin-dir-arg full-proto-file-name)) "-proto.rkt"))
    (define output-dir (build-path output-dir-arg dir-name "compiled"))

    (compile (list source-path) output-dir)))
