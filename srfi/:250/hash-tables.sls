(library (srfi :250 hash-tables)
  (export make-hash-table
          hash-table-size
          hash-table?
          (rename (prefilled-hash-table hash-table))
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

  (import (except (rnrs (6)) vector-fill!)
          (only (rnrs r5rs (6)) modulo)
          (srfi :128 comparators)
          (only (srfi :133 vectors)
                vector-copy!
                vector-fill!)
          (srfi :250 internal include))

  (define-syntax not-on-r6rs
    (syntax-rules ()
      ((_ body_0 body_1 ...) (begin))))
  (define (void . ignored) (if #f #f))
  (define (hash-truncate h) (bitwise-and (abs h) #xFFFFFFFF))

  (define-record-type (hash-table %make-hash-table hash-table?)
    (fields (immutable type-test-function)
            (immutable hash-function)
            (immutable same?-function)
            (mutable size)
            (mutable next-entry)
            (mutable compact-index)
            (mutable keys-vector)
            (mutable values-vector))
    (opaque #t)
    (sealed #t)
    (nongenerative Hash-Table-BE0AFTGAdcwHkSOxhWtxQF+Ai1g))

  (define-syntax define-sentinel
    (syntax-rules ()
      ((_ name pred)
       (begin
         (define-record-type the-sentinel-type)
         (define name (make-the-sentinel-type))
         (define (pred obj) (eq? obj name))))))
  (define-sentinel *unfilled* unfilled?)
  (define-sentinel *deletion* deletion?)
  (define *growth-rate* 3/2)

  (include "srfi/250/internal/r6rs-compact-arrays.scm")
  (include "srfi/250/hash-tables.scm"))
