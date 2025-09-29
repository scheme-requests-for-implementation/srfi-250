(library (srfi :250 internal include)
  (export include)
  (import (rnrs base)
          (rnrs syntax-case)
          (rnrs io ports))
  (define-syntax include
    (lambda (x)
      (define read-file
        (lambda (fn k)
          (let ((p (open-file-input-port fn
                                         (file-options no-create)
                                         'block
                                         (native-transcoder))))
            (let f ((x (get-datum p)))
              (if (eof-object? x)
                  (begin (close-port p) '())
                  (cons (datum->syntax k x)
                        (f (get-datum p))))))))
      (syntax-case x ()
        ((k filename)
         (let ((fn (syntax->datum #'filename)))
           (with-syntax (((exp ...)
                          (read-file fn #'k)))
             #'(begin exp ...))))))))
