(library (srfi :250)
  (export
   ;; Constructors
   make-hash-table
   hash-table
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
   hash-table-xor!)
  (import (srfi :250 hash-tables)))
