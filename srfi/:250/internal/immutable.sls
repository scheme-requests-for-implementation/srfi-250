; SPDX-FileCopyrightText: 2025 Daphne Preston-Kendal
;
; SPDX-License-Identifier: MIT

(library (srfi :250 internal immutable)
  (export (rename (values vector->immutable-vector)
                  (values bytevector->immutable-bytevector)))
  (import (rnrs (6))))
