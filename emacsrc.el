(add-to-list 'load-path "~/.emacs.d/lisp/")

; -- Server --

(require 'server)
(or (server-running-p)
    (server-start))

; -- Window Settings --

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

(scroll-bar-mode -1)

; -- Package Manager --

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(ido
    paredit
    cider
    projectile
    clojure-mode
    haskell-mode
    go-mode
    scala-mode
    haml-mode
    slim-mode
    expand-region))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


; -- Misc --

(load "cider-custom.el")

(load "mathematica.el")
(setq mathematica-command-line "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
(setq mathematica-never-start-kernel-with-mode t)

;(require 'slime)
;(slime-setup '(slime-repl))

(setq visible-bell t)
;(setq ring-bell-function 'ignore)

(defun custom-bell-function ()
  (unless
      (memq this-command
            '(;isearch-abort
              ;abort-recursive-edit
              ;exit-minibuffer
              ;keyboard-quit
              mwheel-scroll
              ;down up
              ;next-line
              ;previous-line
              ;backward-char
              ;forward-char
              ))
    (ding)))
(setq ring-bell-function 'custom-bell-function)

(toggle-word-wrap)
(global-auto-revert-mode)

(setq-default vc-follow-symlinks t)

(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))

(defadvice goto-line (after expand-after-goto-line
                                activate compile)
        "hideshow-expand affected block when using goto-line in a collapsed buffer"
        (save-excursion
           (hs-show-block)))


; -- Functions --

(defun ido-project-find-file ()
  "Use ido to select a file from the project."
  (interactive)
  (let ((ido-list (project-path-cache-get (project-current))))
    (find-file (ido-completing-read "project-files: " ido-list))))

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))


; -- Key Bindings --

(progn
  (global-set-key (kbd "s-/") 'comment-or-uncomment-region)
  (global-set-key (kbd "s-;") 'comment-or-uncomment-region)
  (global-set-key (kbd "s-{") 'previous-multiframe-window)
  (global-set-key (kbd "s-}") 'next-multiframe-window)
  (global-set-key (kbd "s-[") 'previous-multiframe-window)
  (global-set-key (kbd "s-]") 'next-multiframe-window)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "s-b") 'ido-switch-buffer)
  (global-set-key (kbd "s-k") 'kill-current-buffer)

  (global-set-key (kbd "C-z")          nil)
  (global-set-key (kbd "<insert>")     nil)
  (global-set-key (kbd "<insertchar>") nil)

  (global-set-key (kbd "C-=") 'er/expand-region)

  (global-set-key (kbd "s-s") 'save-buffer))

(eval-after-load "projectile"
  '(progn
     (define-key projectile-mode-map (kbd "s-t")
       'projectile-find-file)))

(eval-after-load "clojure-mode"
  '(progn
     (define-key clojure-mode-map (kbd "s-<return>")
       'cider-eval-last-sexp)
     (define-key clojure-mode-map (kbd "s-r")
       'cider-load-current-buffer)
     (define-key clojure-mode-map (kbd "s-R")
       'cider-load-and-eval-again-in-repl)))

(eval-after-load "hideshow"
  '(progn
     (define-key hs-minor-mode-map (kbd "s-1") 'hs-toggle-hiding)
     (define-key hs-minor-mode-map (kbd "s-2") 'hs-hide-level)
     (define-key hs-minor-mode-map (kbd "s-3") 'hs-hide-all)
     (define-key hs-minor-mode-map (kbd "s-4") 'hs-show-all)))

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-M-<backspace>") 'backward-kill-sexp)))

(add-hook 'c-mode-common-hook
  (lambda ()
    (local-set-key (kbd "C-c o") 'ff-find-other-file)))


; -- Hooks --

(require 'highlight-sexps)
(add-hook 'clojure-mode-hook 'highlight-sexps-mode)

(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode t)))
(add-hook 'go-mode-hook       (lambda () (setq indent-tabs-mode t)))

(defun delete-trailing-whitespace-on-save ()
  (add-hook 'local-write-file-hooks
    (lambda ()
      (save-excursion
        (delete-trailing-whitespace)))))

(add-hook 'ruby-mode-hook     'delete-trailing-whitespace-on-save)
(add-hook 'js-mode-hook       'delete-trailing-whitespace-on-save)
(add-hook 'clojure-mode-hook  'delete-trailing-whitespace-on-save)
(add-hook 'c-mode-common-hook 'delete-trailing-whitespace-on-save)

(add-hook 'ruby-mode-hook     'hs-minor-mode)

(defun fold-if-long-file ()
  (hs-minor-mode)
  (if (> (buffer-size) 2048)
      (hs-hide-all)))

;(add-hook 'clojure-mode-hook  'fold-if-long-file)

; -- Auto-Mode Setup --

(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("/Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("/mutt-.*-.*$" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))


; -- Customizations --

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs-backup" t))))
 '(auto-save-list-file-prefix "~/.emacs-backup.auto-saves-")
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs-backup"))))
 '(cider-popup-on-error nil)
 '(cider-popup-stacktraces nil)
 '(cider-prompt-save-file-on-load nil)
 '(clojure-defun-indents (quote (GET POST DELETE this-as)))
 '(custom-enabled-themes (quote (wombat-mod)))
 '(custom-safe-themes (quote ("60a0eafa8dc70f464d574c2630ef712d832679f10095a87bae37166200ad0f76" default)))
 '(dired-use-ls-dired nil)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(hl-sexp-background-colors (quote ("#353535")))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-file-extensions-order (quote (".hs" ".clj" ".rb" ".c" ".js" ".txt" ".emacs")))
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(nrepl-connected-hook (quote (cider-enable-on-existing-clojure-buffers cider-display-connected-message)))
 '(nrepl-disconnected-hook (quote (cider-possibly-disable-on-existing-clojure-buffers)))
 '(nrepl-host "localhost")
 '(nrepl-port 7888)
 '(project-mode t)
 '(project-search-exclusion-regexes-default (quote ("[\\\\/]SCCS[\\\\/]" "[\\\\/]RCS[\\\\/]" "[\\\\/]CVS[\\\\/]" "[\\\\/]MCVS[\\\\/]" "[\\\\/]\\.svn[\\\\/]" "[\\\\/]\\.git[\\\\/]" "[\\\\/]\\.hg[\\\\/]" "[\\\\/]\\.bzr[\\\\/]" "[\\\\/]_MTN[\\\\/]" "[\\\\/]_darcs[\\\\/]" "[\\\\/].#" "\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.pfsl$" "\\.dfsl$" "\\.p64fsl$" "\\.d64fsl$" "\\.dx64fsl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$" "\\.jar$" "\\.class$" "\\.exe$" "\\.png$" "\\.gif$" "\\.jpg$" "\\.jpeg$" "\\.ico$" "\\.rtf$" "\\.tar$" "\\.tgz$" "\\.gz$" "\\.bz2$" "\\.zip$" "\\.rar$" "\\.cab$" "\\.dll$" "\\.pdf$" "\\.tmp$" "\\.log$" "\\.msi$" "\\.war$" "\\bTAGS$" "\\.hi$" "\\.DS_Store$")))
 '(projectile-global-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
