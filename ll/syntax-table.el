(defconst ll/syntax-table
  (let ((result (make-syntax-table)))
    ; Comments
    (modify-syntax-entry ?\; "< b" result)
    (modify-syntax-entry ?\n "> b" result)

    ; Words
    (modify-syntax-entry ?% "w" result)
    (modify-syntax-entry ?. "w" result)

    result)
  "Syntax table for LL mode")

(provide 'll/syntax-table)
