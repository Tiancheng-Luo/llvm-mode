(defconst llvm/syntax-table
  (let ((result (make-syntax-table)))
    ; Comments
    (modify-syntax-entry ?\; "< b" result)
    (modify-syntax-entry ?\n "> b" result)

    ; Words
    (modify-syntax-entry ?@ "w" result)
    (modify-syntax-entry ?% "w" result)
    (modify-syntax-entry ?. "w" result)
    (modify-syntax-entry ?! "w" result)
    (modify-syntax-entry ?: "w" result)
    (modify-syntax-entry ?# "w" result)

    result)
  "Syntax table for `llvm-mode'")

(provide 'llvm/syntax-table)
