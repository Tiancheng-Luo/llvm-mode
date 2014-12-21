(defcustom llvm/indent/offset 4
  "Default indentation offset for `llvm-mode'."
  :group 'll
  :safe 'integerp
  :type 'integer)

(defun llvm/indent-line-function ()
  "`indent-line-function' for `llvm-mode'."

  (let ((left  (llvm/occurrences-backward "{"))
        (right (llvm/occurrences-backward "}")))

    (if (not (< left right))
      (indent-line-to (* llvm/indent/offset (- left right))))))

;; Helpers

(defun llvm/occurrences-forward  (string) (llvm/_occurrences string #'search-forward))
(defun llvm/occurrences-backward (string) (llvm/_occurrences string #'search-backward))

(defun llvm/_occurrences (string search)
  (save-excursion
    (let (
           (count 0)
           (prev (point))
           (curr (funcall search string nil t))
           )

      (while curr
        (setq prev curr)
        (setq curr (funcall search string nil t))
        (setq count (1+ count)))

      count)))

(provide 'llvm/indent-line-function)
