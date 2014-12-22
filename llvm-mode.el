(defun llvm-mode ()
  "Major mode for editing LLVM assembly."
  (interactive)
  (kill-all-local-variables)

  (require 'llvm/font-lock-defaults)
  (require 'llvm/indent-line-function)
  (require 'llvm/syntax-table)

  (set (make-local-variable `font-lock-defaults)  '(llvm/font-lock-defaults))
  (set (make-local-variable `indent-line-function) 'llvm/indent-line-function)

  (set-syntax-table llvm/syntax-table)

  (setq major-mode 'llvm-mode)
  (setq mode-name "LLVM mode")

  (run-hooks 'llvm-mode-hook)

  (font-lock-fontify-buffer))

(defvar llvm-mode-hook
  nil
  "Hook for `llvm-mode.")

(provide 'llvm-mode)
