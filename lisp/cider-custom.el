(defun cider-eval-again-in-repl ()
  (interactive)
  (with-current-buffer (cider-find-or-create-repl-buffer)
    (cider-repl--replace-input (car cider-repl-input-history))
    (goto-char (point-max))
    (cider-repl-return)
    (goto-char (point-max))
    ))

(defun cider-eval-again-in-repl-remove-hook ()
  (cider-eval-again-in-repl)
  (remove-hook 'cider-file-loaded-hook 'cider-eval-again-in-repl-remove-hook))

(defun cider-load-and-eval-again-in-repl ()
  (interactive)
  (add-hook 'cider-file-loaded-hook 'cider-eval-again-in-repl-remove-hook 't)
  (cider-load-current-buffer))
