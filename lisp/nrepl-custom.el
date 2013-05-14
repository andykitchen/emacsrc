;; (add-hook 'nrepl-interaction-mode-hook
;;   'nrepl-turn-on-eldoc-mode)

(add-to-list 'same-window-buffer-names "*nrepl*")

(defun nrepl-save-and-load-current-buffer ()
  (interactive)
  (save-buffer)
  (nrepl-load-current-buffer))

(defcustom nrepl-test-namespaces "'example.ns"
  "namespaces to run tests on"
  :type  'string
  :group 'nrepl)

(make-variable-buffer-local 'nrepl-test-namespaces)

;; TODO Fixme
(defun nrepl-run-tests ()
  (interactive)
  (save-buffer)
  (nrepl-load-current-buffer)
  (let ((namespaces nrepl-test-namespaces))
    (with-current-buffer (get-buffer "*nrepl*")
      (goto-char (point-max))
      (insert "(clojure.test/run-tests " namespaces ")")
      (nrepl-return t))))

(defun run-again-nrepl-load-file-handler (buffer)
  (let (current-ns (nrepl-current-ns))
    (nrepl-make-response-handler
     buffer
     (lambda (buffer value)
       (message "%s" value)
       (with-current-buffer buffer
         (setq nrepl-buffer-ns (clojure-find-ns))))
     (lambda (buffer value)
       (nrepl-emit-interactive-output value))
     (lambda (buffer err)
       (message "%s" err))
     (lambda (buffer)
       (with-current-buffer (get-buffer "*nrepl*")
         (nrepl-replace-input (car nrepl-input-history))
         (nrepl-send-input t))))))

(defun run-again-nrepl-send-load-file (file-contents file-path file-name)
  (let ((buffer (current-buffer)))
    (nrepl-send-request (list "op"        "load-file"
                              "session"   (nrepl-current-session)
                              "file"      file-contents
                              "file-path" file-path
                              "file-name" file-name)
                        (run-again-nrepl-load-file-handler buffer))))

(defun run-again-nrepl-load-file-op (filename)
  (run-again-nrepl-send-load-file (nrepl-file-string filename)
                               filename
                               (file-name-nondirectory filename)))

(defun nrepl-run-again ()
  (interactive)
  (save-buffer)
  (run-again-nrepl-load-file-op (buffer-file-name)))
