; SPDX-FileCopyrightText: 2025 Daphne Preston-Kendal
;
; SPDX-License-Identifier: MIT

(library (srfi :250 internal immutable)
  (export vector->immutable-vector
          bytevector->immutable-bytevector)
  (import (chezscheme)))
