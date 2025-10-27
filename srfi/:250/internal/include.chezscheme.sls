; SPDX-FileCopyrightText: 2025 Daphne Preston-Kendal
;
; SPDX-License-Identifier: MIT

;; prefer Chez’s native ‘include’, which tracks source location
;; information correctly
(library (srfi :250 internal include)
  (export include)
  (import (chezscheme)))
