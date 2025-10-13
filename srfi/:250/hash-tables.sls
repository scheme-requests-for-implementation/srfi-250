(library (srfi :250 hash-tables)
  (export
   ;; Constructors
   make-hash-table
   (rename (prefilled-hash-table hash-table))
   hash-table-unfold
   alist->hash-table
   ;; Predicates
   hash-table?
   hash-table-contains?
   hash-table-empty?
   hash-table-mutable?
   ;; Accessors
   hash-table-ref
   hash-table-ref/default
   hash-table-comparator
   ;; Mutators
   hash-table-add!
   hash-table-replace!
   hash-table-set!
   hash-table-delete!
   hash-table-intern!
   hash-table-update!
   hash-table-update!/default
   hash-table-pop!
   hash-table-clear!
   ;; The whole hash table
   hash-table-size
   hash-table=
   hash-table-find
   hash-table-count
   hash-table-keys
   hash-table-values
   hash-table-entries
   ;; Low-level iteration
   hash-table-cursor-first
   hash-table-cursor-last
   hash-table-cursor-for-key
   hash-table-cursor-next
   hash-table-cursor-previous
   hash-table-cursor-key
   hash-table-cursor-value
   hash-table-cursor-key+value
   hash-table-cursor-value-set!
   hash-table-cursor-at-end?
   ;; Mapping and folding
   hash-table-map
   hash-table-map!
   hash-table-for-each
   hash-table-map->list
   hash-table-fold
   hash-table-fold-left
   hash-table-fold-right
   hash-table-prune!
   ;; Copying and conversion
   hash-table-copy
   hash-table-empty-copy
   hash-table->alist
   ;; Hash tables as sets
   hash-table-union!
   hash-table-intersection!
   hash-table-difference!
   hash-table-xor!)

  (import (except (rnrs (6)) vector-fill!)
          (only (rnrs r5rs (6)) modulo)
          (srfi :128 comparators)
          (only (srfi :133 vectors)
                vector-copy!
                vector-fill!)
          (srfi :250 internal include)
          (srfi :250 internal immutable))

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
            (mutable compact-index-max-fill)
            (mutable keys-vector)
            (mutable values-vector)
            (mutable mutable?))
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

  (define *default-k* 7)
  (define *growth-rate* 3/2)

  (define (hash-table-immutablize! ht)
    (hash-table-keys-vector-set! ht (vector->immutable-vector
                                     (hash-table-keys-vector ht)))
    (hash-table-values-vector-set! ht (vector->immutable-vector
                                       (hash-table-values-vector ht)))
    (hash-table-compact-index-set! ht (bytevector->immutable-bytevector
                                       (hash-table-compact-index ht)))
    (hash-table-mutable?-set! ht #f))

  (define (vector-copy vec)
    (define len (vector-length vec))
    (let ((out-vec (make-vector len)))
      (let loop ((idx 0))
        (when (< idx len)
          (vector-set! out-vec idx (vector-ref vec idx))
          (loop (+ idx 1))))
      out-vec))

  (include "srfi/250/internal/r6rs-compact-arrays.scm")
  (include "srfi/250/hash-tables.scm"))
