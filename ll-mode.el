(defun ll-mode ()
  "Major mode for editing LLVM assembly."
  (interactive)
  (kill-all-local-variables)

  (require 'll/font-lock-defaults)
  (require 'll/indent-line-function)
  (require 'll/syntax-table)

  (set (make-local-variable `font-lock-defaults)  '(ll/font-lock-defaults))
  (set (make-local-variable `indent-line-function) 'll/indent-line-function)

  (set-syntax-table ll/syntax-table)

  (setq major-mode 'll-mode)
  (setq mode-name "LL mode")

  (run-hooks 'll-mode-hook))

(defvar ll-mode-hook
  nil
  "Hook for LL mode")

(provide 'll-mode)
