;; SRFI 250 implementation (with prelude) for Guile

(define-module (srfi srfi-250)
  #:use-module ((rnrs)
                #:version (6))
  #:use-module ((scheme base)
                #:select (modulo
                          vector-copy
                          vector-copy!
                          vector-fill!))
  #:use-module ((guile) #:select (include))
  #:use-module ((srfi srfi-9 gnu) #:select (set-record-type-printer!))
  #:use-module (srfi srfi-128) ; https://codeberg.org/pukkamustard/guile-srfi-128
  #:duplicates (last)
  #:pure
  #:declarative? #t
  #:export (;; Constructors
            make-hash-table
            (prefilled-hash-table . hash-table)
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
            hash-table-xor!))

(define-syntax not-on-r6rs
  (syntax-rules ()
    ((_ body_0 body_1 ...) (begin body_0 body_1 ...))))

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
          (mutable values-vector)
          (mutable mutable?))
  (opaque #t)
  (sealed #t)
  (nongenerative Hash-Table-BE0AFTGAdcwHkSOxhWtxQF+Ai1g))

(define *unfilled*
  (let ()
    (define-record-type (Unfilled make-unfilled unfilled?))
    (make-unfilled)))
(define (unfilled? obj) (eq? obj *unfilled*))

(define *deletion*
  (let ()
    (define-record-type (Deletion make-deletion deletion?))
    (make-deletion)))
(define (deletion? obj) (eq? obj *deletion*))

(define *default-k* 7)
(define *growth-rate* 3/2)

(define (hash-table-immutablize! ht)
  (hash-table-mutable?-set! ht #f))

(include "250/internal/r6rs-compact-arrays.scm")
(include "250/hash-tables.scm")

(set-record-type-printer! (record-type-descriptor hash-table)
  (lambda (ht port)
    (display "#<hash-table with " port)
    (display (hash-table-size ht) port)
    (display " entr" port)
    (if (= (hash-table-size ht) 1)
        (display "y (" port)
        (display "ies (" port))
    (let loop ((n 0)
               (cur (hash-table-cursor-first ht)))
      (cond ((and (>= n 6)
                  (not (hash-table-cursor-at-end? ht cur)))
             (display " ...)>" port))
            ((hash-table-cursor-at-end? ht cur)
             (display ")>" port))
            (else
             (if (> n 0) (display #\space port))
             (let ((pair (call-with-values
                             (lambda () (hash-table-cursor-key+value ht cur))
                           cons)))
               (write pair port)
               (loop (+ n 1)
                     (hash-table-cursor-next ht cur))))))))
