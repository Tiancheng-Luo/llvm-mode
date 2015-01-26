(require 'llvm/font-lock-keywords)
(require 'llvm/indent-line)
(require 'llvm/syntax-table)

(defcustom llvm-mode-hook nil
  "Normal hook run when entering `llvm-mode'."
  :group 'llvm/options
  :type 'hook)

(defun llvm-mode ()
  "Major mode for editing LLVM assembly."
  (interactive)
  (kill-all-local-variables)

  (set
    (make-local-variable `font-lock-defaults)
    '(llvm/font-lock-keywords))

  (set
    (make-local-variable `indent-line-function)
    'llvm/indent-line)

  (set-syntax-table
    llvm/syntax-table)

  (setq
    major-mode
    'llvm-mode)

  (setq
    mode-name
    "LLVM mode")

  (font-lock-fontify-buffer)

  (run-hooks
    'llvm/hook))

(provide 'llvm-mode)
