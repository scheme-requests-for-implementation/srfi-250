;; prefer Chez’s native ‘include’, which tracks source location
;; information correctly
(library (srfi :250 internal include)
  (export include)
  (import (chezscheme)))
