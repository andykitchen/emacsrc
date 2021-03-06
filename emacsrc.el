(add-to-list 'load-path "~/.emacs.d/lisp/")

; -- Path --

(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))


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
    helm
    projectile
    helm-projectile
    paredit
    rainbow-delimiters
    hippie-expand-slime
    expand-region
    slime
    cider
    geiser
    clojure-mode
    haskell-mode
    go-mode
    ;scala-mode
    ;haml-mode
    ;slim-mode
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


; -- Misc --

(toggle-word-wrap)
(global-auto-revert-mode)

(setq-default vc-follow-symlinks t)


; -- Bell --

(setq visible-bell t)
;(setq ring-bell-function 'ignore)

(defun custom-bell-function ()
  (unless
      (memq this-command
            '(isearch-abort
              abort-recursive-edit
              exit-minibuffer
              keyboard-quit
              mwheel-scroll
              down up
              next-line
              previous-line
              backward-char
              forward-char
              ))
    (ding)))
(setq ring-bell-function 'custom-bell-function)


; -- Package: Hide/Show --

(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))

(defadvice goto-line (after expand-after-goto-line
                            activate
                            compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))


; -- Package: Mathematica --

(autoload 'mathematica-mode "mathematica" "Mathematica major mode" t)

(eval-after-load "mathematica"
  '(progn
     (setq mathematica-command-line "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
     (setq mathematica-never-start-kernel-with-mode t)))


; -- Package: Extempore --

(autoload 'extempore-mode "extempore" "Extempore major mode" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

(eval-after-load "extempore"
  '(progn
     (define-key extempore-mode-map (kbd "s-<return>") 'extempore-send-definition)))


; -- Package: Highlight-Sexps

(require 'highlight-sexps)

; -- Package: Hippie Expand

; (global-set-key "\M- " 'hippie-expand)

; -- Package: Helm

(require 'helm)
(require 'helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)

; (global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)


; -- SBCL

(setq inferior-lisp-program "sbcl")


; -- Cider Customizations

(eval-after-load "cider" '(load "cider-custom"))


; -- Comint Mode Clear

(defun comint-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(eval-after-load "comint-mode"
  '(progn
     (define-key comint-mode-map (kbd "C-c M-o") 'comint-clear)))


; -- Lisp

(eval-after-load "lisp-mode"
  '(progn
     (define-key lisp-mode-map (kbd "s-<return>") 'slime-eval-last-expression)))


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
       'cider-eval-last-sexp)))

(eval-after-load "cider-mode"
  '(progn
     (define-key cider-mode-map (kbd "s-r")
       'cider-load-buffer)
     (define-key cider-mode-map (kbd "s-R")
       'cider-load-and-eval-again-in-repl)))

;; (eval-after-load "cider-mode"
;;   '(progn
;;      (define-key cider-mode-map (kbd "s-r")
;;        'cider-tools-namespace-refresh)
;;      (define-key cider-mode-map (kbd "s-R")
;;        'cider-tools-namespace-refresh-and-eval-again)))

(eval-after-load "hideshow"
  '(progn
     (define-key hs-minor-mode-map (kbd "s-1") 'hs-toggle-hiding)
     (define-key hs-minor-mode-map (kbd "s-2") 'hs-hide-level)
     (define-key hs-minor-mode-map (kbd "s-3") 'hs-hide-all)
     (define-key hs-minor-mode-map (kbd "s-4") 'hs-show-all)))

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-M-<backspace>") 'backward-kill-sexp)))

(eval-after-load "geiser"
  '(progn
     (define-key scheme-mode-map (kbd "s-<return>") 'geiser-eval-last-sexp)))

(add-hook 'c-mode-common-hook
  (lambda ()
    (local-set-key (kbd "C-c o") 'ff-find-other-file)))


; -- Hooks --

(defun add-hooks-for-lisp (hook)
  (add-hook hook 'paredit-mode)
  (add-hook hook 'highlight-sexps-mode)
  (add-hook hook 'rainbow-delimiters-mode)
  (add-hook hook 'hs-minor-mode))

(add-hooks-for-lisp 'lisp-mode-hook)
(add-hooks-for-lisp 'emacs-lisp-mode-hook)
(add-hooks-for-lisp 'scheme-mode-hook)
(add-hooks-for-lisp 'clojure-mode-hook)

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)

(add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode t)))
(add-hook 'go-mode-hook       (lambda () (setq indent-tabs-mode t)))

(defun delete-trailing-whitespace-on-save ()
  (add-hook 'local-write-file-hooks
    (lambda ()
      (save-excursion
        (delete-trailing-whitespace)))))

(add-hook 'lisp-mode-hook     'delete-trailing-whitespace-on-save)
(add-hook 'ruby-mode-hook     'delete-trailing-whitespace-on-save)
(add-hook 'js-mode-hook       'delete-trailing-whitespace-on-save)
; (add-hook 'clojure-mode-hook  'delete-trailing-whitespace-on-save)
(add-hook 'c-mode-common-hook 'delete-trailing-whitespace-on-save)

(add-hook 'ruby-mode-hook     'hs-minor-mode)

(defun fold-if-long-file ()
  (hs-minor-mode)
  (if (> (buffer-size) 2048)
      (hs-hide-all)))

;(add-hook 'clojure-mode-hook  'fold-if-long-file)

; -- Indent React.DOM calls correctly --

(eval-after-load "clojure-mode"
  '(dolist (sym '(div h1 ul li a i span))
     (put-clojure-indent sym 'defun)))

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
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs-backup" t))))
 '(auto-save-list-file-prefix "~/.emacs-backup.auto-saves-")
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs-backup"))))
 '(cider-popup-on-error nil)
 '(cider-prompt-save-file-on-load nil)
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(cider-repl-use-clojure-font-lock t)
 '(cider-show-error-buffer t)
 '(clojure-defun-indents
   (quote
    (GET POST DELETE this-as describe it it* fn-props fn-props-state fact facts)))
 '(custom-enabled-themes (quote (wombat-mod)))
 '(custom-safe-themes
   (quote
    ("60a0eafa8dc70f464d574c2630ef712d832679f10095a87bae37166200ad0f76" default)))
 '(dired-use-ls-dired nil)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(helm-mode t)
 '(hl-sexp-background-colors (quote ("#353535")))
 '(ido-file-extensions-order (quote (".hs" ".clj" ".cljs" ".rb" ".c" ".txt" ".emacs" t)))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(nrepl-connected-hook
   (quote
    (cider-enable-on-existing-clojure-buffers cider-display-connected-message)))
 '(nrepl-disconnected-hook
   (quote
    (cider-possibly-disable-on-existing-clojure-buffers)))
 '(project-mode t)
 '(project-search-exclusion-regexes-default
   (quote
    ("[\\\\/]SCCS[\\\\/]" "[\\\\/]RCS[\\\\/]" "[\\\\/]CVS[\\\\/]" "[\\\\/]MCVS[\\\\/]" "[\\\\/]\\.svn[\\\\/]" "[\\\\/]\\.git[\\\\/]" "[\\\\/]\\.hg[\\\\/]" "[\\\\/]\\.bzr[\\\\/]" "[\\\\/]_MTN[\\\\/]" "[\\\\/]_darcs[\\\\/]" "[\\\\/].#" "\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.pfsl$" "\\.dfsl$" "\\.p64fsl$" "\\.d64fsl$" "\\.dx64fsl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$" "\\.jar$" "\\.class$" "\\.exe$" "\\.png$" "\\.gif$" "\\.jpg$" "\\.jpeg$" "\\.ico$" "\\.rtf$" "\\.tar$" "\\.tgz$" "\\.gz$" "\\.bz2$" "\\.zip$" "\\.rar$" "\\.cab$" "\\.dll$" "\\.pdf$" "\\.tmp$" "\\.log$" "\\.msi$" "\\.war$" "\\bTAGS$" "\\.hi$" "\\.DS_Store$")))
 '(projectile-global-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
