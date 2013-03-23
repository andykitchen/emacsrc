;; (provide 'nrepl-custom)

;; (add-hook 'nrepl-interaction-mode-hook
;;   'nrepl-turn-on-eldoc-mode)

(add-to-list 'same-window-buffer-names "*nrepl*")

(defun my-nrepl-err-handler (buffer ex root-ex session)
  (with-current-buffer buffer
    (nrepl-send-string 
     "(if-let [pst+ (clojure.core/resolve 'clj-stacktrace.repl/pst+)]
                      (pst+ *e) (clojure.stacktrace/print-stack-trace *e))"
     (nrepl-make-response-handler
      (nrepl-make-popup-buffer nrepl-error-buffer)
      nil
      'nrepl-emit-into-color-buffer nil nil) nil session)))

(eval-after-load "nrepl"
  '(progn
     (setq-default nrepl-err-handler 'my-nrepl-err-handler)))

(defun nrepl-save-and-load-current-buffer ()
  (interactive)
  (save-buffer)
  (nrepl-load-current-buffer))

(defcustom nrepl-test-namespaces "'example.ns"
  "namespaces to run tests on"
  :type  'string
  :group 'nrepl)

(make-variable-buffer-local 'nrepl-test-namespaces)

(defun nrepl-run-tests ()
  (interactive)
  (save-buffer)
  (nrepl-load-current-buffer)
  (let ((namespaces nrepl-test-namespaces))
    (with-current-buffer (get-buffer "*nrepl*")
      (goto-char (point-max))
      (insert "(clojure.test/run-tests " namespaces ")")
      (nrepl-return t))))

(defun nrepl-run-again ()
  (interactive)
  (save-buffer)
  (nrepl-load-current-buffer)
  (with-current-buffer (get-buffer "*nrepl*")
    (nrepl-replace-input (car nrepl-input-history))
    (nrepl-return t)))
