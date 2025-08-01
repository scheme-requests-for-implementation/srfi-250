(define-library (srfi 250)
  (export make-hash-table
          hash-table-size
          hash-table?
          (rename prefilled-hash-table hash-table)
          alist->hash-table
          hash-table-set!
          hash-table-delete!
          hash-table-clear!
          hash-table-ref
          hash-table-ref/default
          hash-table-contains?
          hash-table-empty?
          hash-table-fold
          hash-table-fold-right
          hash-table-for-each
          hash-table->alist
          hash-table-find
          hash-table-count
          hash-table-prune!
          hash-table-union!
          hash-table-intersection!
          hash-table-difference!
          hash-table-xor!
          hash-table=
          hash-table-unfold
          hash-table-intern!
          hash-table-update!
          hash-table-update!/default)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write) ; nocommit
          
          (srfi 1)
          (srfi 128)
          (srfi 151)
          (srfi 160 base))

  (begin
    (define-syntax not-on-r6rs
      (syntax-rules ()
        ((_ body_0 body_1 ...) (begin body_0 body_1 ...))))

    (define (void . ignored) (if #f #f))
    (define (hash-truncate h) (bitwise-and (abs h) #xFFFFFFFF))

    (define-record-type Hash-Table
      (%make-hash-table type-test-function hash-function same?-function
                        size next-entry compact-index
                        keys-vector values-vector)
      hash-table?
      (type-test-function hash-table-type-test-function)
      (hash-function hash-table-hash-function)
      (same?-function hash-table-same?-function)
      (size hash-table-size hash-table-size-set!)
      (next-entry hash-table-next-entry hash-table-next-entry-set!)
      (compact-index hash-table-compact-index hash-table-compact-index-set!)
      (keys-vector hash-table-keys-vector hash-table-keys-vector-set!)
      (values-vector hash-table-values-vector hash-table-values-vector-set!))

    (define *unfilled*
      (let ()
        (define-record-type Unfilled (make-unfilled) unfilled?)
        (make-unfilled)))
    (define (unfilled? obj) (eq? obj *unfilled*))

    (define *deletion*
      (let ()
        (define-record-type Deletion (make-deletion) deletion?)
        (make-deletion)))
    (define (deletion? obj) (eq? obj *deletion*))

    (define *growth-rate* 3/2)

    (define (assertion-violation who message irritants)
      (apply error
             (string-append (if (symbol? who)
                                (symbol->string who)
                                who)
                            ": "
                            message)
             irritants)))
  (include "250/internal/srfi-compact-arrays.scm")
  (include "250/hash-tables.scm"))
