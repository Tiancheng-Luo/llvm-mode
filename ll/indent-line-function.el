(defcustom ll/indent/offset 4
  "Default indentation offset for `ll-mode'."
  :group 'll
  :safe 'integerp
  :type 'integer)

(defun ll/indent-line-function ()
  "`indent-line-function' for LL mode."

  (let ((left  (ll/occurrences-backward "{"))
        (right (ll/occurrences-backward "}")))

    (if (> left right)
      (indent-line-to (* ll/indent/offset (- left right))))))

;; Helpers

(defun ll/occurrences-forward  (string) (ll/_occurrences string #'search-forward))
(defun ll/occurrences-backward (string) (ll/_occurrences string #'search-backward))

(defun ll/_occurrences (string search)
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

(provide 'll/indent-line-function)
