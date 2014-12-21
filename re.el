(defun re/keyword (&rest strings)
  "Construct a regular expression which matches, as a word, any of `strings`."
  (concat "\\<" (regexp-opt strings) "\\>"))

(provide 're)
