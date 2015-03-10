(defun cider-eval-again-in-repl ()
  (interactive)
  (with-current-buffer (cider-get-repl-buffer)
    (cider-repl--replace-input
     (or (car cider-repl-input-history) ""))
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
  (cider-load-buffer))

(defun cider-refresh-eval-again-handler (&optional buffer)
  "Make an interactive eval handler for BUFFER."
  (nrepl-make-response-handler (or buffer (current-buffer))
                               (lambda (_buffer value)
                                 (message "%s%s"
                                          cider-interactive-eval-result-prefix
                                          (cider-font-lock-as-clojure value))
                                 (cider-eval-again-in-repl))
                               (lambda (_buffer out)
                                 (cider-emit-interactive-eval-output out))
                               (lambda (buffer err)
                                 (cider-emit-interactive-eval-err-output err)
                                 (cider-handle-compilation-errors err buffer))
                               '()))

(defconst cider-reset-form
   "
   (let [reset (if-let [user (find-ns 'user)]
                 (ns-resolve user 'reset)
                 #(clojure.tools.namespace.repl/refresh :after 'user/go))
         ret   (reset)]
       (when (instance? java.lang.Throwable ret)
         (throw ret))
       ret)
   ")

(defun cider-tools-namespace-refresh ()
  (interactive)
  (save-buffer)
  (cider-interactive-eval
   cider-reset-form))

(defun cider-tools-namespace-refresh-and-eval-again ()
  (interactive)
  (save-buffer)
  (cider-interactive-eval
   cider-reset-form
   (cider-refresh-eval-again-handler)))
