;; MELPA (latest)
;; https://www.emacswiki.org/emacs/MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))
;; IF YOU ARE HAVING DIFFICULTIES GETTING PACKAGES, UNCOMMENT BELOW LINE
;; It's necessary any time I add a new package or install for the first time
;;(package-refresh-contents) ;; Required to maintain updated package list

;; ******************************************
;; ---~~~====  SUPPORTING LIBRARIES  =====~~~---
;; ******************************************
;; A modern list library -- https://github.com/magnars/dash.el
;; REQUIRED FOR SOLARIZED THEME
(unless (package-installed-p 'dash) (package-install 'dash))

;; ************************************
;; ---~~~====  GENERAL CONFIG  =====~~~---
;; ************************************

;; OSX: Resize to default MacBook Pro 2015 screensize
(setq initial-frame-alist
      `((left . 60) (top . 0)
	(width . 80) (height . 56)))

;; OSX: Disable Emacs GUI
(add-hook 'desktop-after-read-hook (lambda () (tool-bar-mode -1)))

(setq ns-pop-up-frames nil)

;; Tell Emacs to only GC when 40mb of garbage is reached
;; this prevents aggressive GCs that trigger several time a second and create bad UX
;; (If the current year is >=2020, you should probably increase this)
(setq gc-cons-threshold 40000000)

;; TODO: fix colors being off in from `ansi-term` command
;; TODO: refactor all package declarations to `use-package' format for cleanliness+faster boot
;; TODO: https://github.com/purcell/exec-path-from-shell
;; TODO: enable below Hippie-expand, except it doesn't seem to be a MELPA package?
;; TODO: switch to previous buffer: http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/

;; Starts the emacsclient server -- forces Emacs to run as a daemon
;; With the proper OS configuration of emacsclient, you can run "emacs filename.txt"
;; To open a file into the running Emacs window on OSX
;; ref https://stackoverflow.com/questions/10171280/how-to-launch-gui-emacs-from-command-line-in-osx
(server-start)
(x-focus-frame nil) ;; required for proper OSX behavior

;; Makes Emacs remember open buffers between runs
(desktop-save-mode 1)

;; Disable logging messages when font-locking to speed up rendering
(setq font-lock-verbose nil)

;; Always show column position
(setq column-number-mode t)

;; [use-package] - streamlined package specification - https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package) (package-install 'use-package))

;; [buffer-move] - transpose buffers - https://github.com/lukhas/buffer-move
(unless (package-installed-p 'buffer-move) (package-install 'buffer-move))
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; [Helm] - incremental completion and selection narrowing - https://github.com/emacs-helm/helm
(unless (package-installed-p 'helm) (package-install 'helm))
(require 'helm-config)
(helm-mode 1)
;; Replace some built-in FNs with the helm upgrades
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; [Hippie Expand] - Tries to expand word at/before point
;; (unless (package-installed-p 'hippie-expand) (package-install 'hippie-expand))

;; [Flycheck] - inline error highlighting - http://www.flycheck.org/en/latest/
(unless (package-installed-p 'flycheck) (package-install 'flycheck))
(global-flycheck-mode)
;; OSX workaround for Flycheck - https://github.com/purcell/exec-path-from-shell
(unless (package-installed-p 'exec-path-from-shell) (package-install 'exec-path-from-shell))

;; [Rainbow Delimiters] - multicolor parens - https://www.emacswiki.org/emacs/RainbowDelimiters
(unless (package-installed-p 'rainbow-delimiters) (package-install 'rainbow-delimiters))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; [Rainbow Mode] - color-code hexstrings - https://github.com/emacsmirror/rainbow-mode
(unless (package-installed-p 'rainbow-mode) (package-install 'rainbow-mode))

;; [Solarized-theme] - https://github.com/bbatsov/solarized-emacs
;;(unless (package-installed-p 'solarized-theme) (package-install 'solarized-theme))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/lisp/")
;;(setq solarized-distinct-doc-face t)

;; [Desktop+] - save/load sets of files into buffers - https://github.com/ffevotte/desktop-plus
(unless (package-installed-p 'desktop+) (package-install 'desktop+))

;; [Projectile] - 1st class abstractions on project files - https://github.com/bbatsov/projectile
(unless (package-installed-p 'projectile) (package-install 'projectile))
(projectile-global-mode) ;; enabled for all modes

;; [Helm-Projectile] - Helm completion in Projectile commands - https://github.com/bbatsov/helm-projectile
(unless (package-installed-p 'helm-projectile) (package-install 'helm-projectile))
(require 'helm-projectile)
(helm-projectile-on)

;; [Aggressive Indent Mode] - https://github.com/Malabarba/aggressive-indent-mode
(unless (package-installed-p 'aggressive-indent) (package-install 'aggressive-indent))
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; [Modern Paredit for major modes] - https://github.com/Fuco1/smartparens
;;  - has support for Paredit in non-LISP modes, but not yet understood/enabled
(unless (package-installed-p 'smartparens) (package-install 'smartparens))
(require 'smartparens-config) ;; loads default bindings & settings
;; reference: https://github.com/Fuco1/smartparens/blob/master/smartparens-config.el
(add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode) ;; ???
;; NOTE: there are many smartparens bindings at the end of this file

(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'show-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook       #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook       #'show-smartparens-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-strict-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'show-smartparens-mode)
(add-hook 'ielm-mode-hook             #'smartparens-strict-mode)
(add-hook 'ielm-mode-hook             #'show-smartparens-mode)
(add-hook 'lisp-mode-hook             #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook             #'show-smartparens-mode)
(add-hook 'lisp-interaction-mode-hook #'smartparens-strict-mode)
(add-hook 'lisp-interaction-mode-hook #'show-smartparens-mode)
(add-hook 'scheme-mode-hook           #'smartparens-strict-mode)
(add-hook 'scheme-mode-hook           #'show-smartparens-mode)

;; [Hydra] - tie together related commands off common prefix - https://github.com/abo-abo/hydra
(unless (package-installed-p 'hydra) (package-install 'hydra))

;; [Modern Regex] - Provides modern regex cmds - www.github.com/benma/visual-regexp-steroids.el
(unless (package-installed-p 'visual-regexp-steroids)
  (package-install 'visual-regexp-steroids))

;; [ggtags] - improves Emacs symbol tagging - https://github.com/leoliu/ggtags
(unless (package-installed-p 'ggtags)
  (package-install 'ggtags))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'clojure-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; ************************************
;; ---~~~====  CLOJURE CONFIG  ====~~~---
;; ************************************

;; [lein config] - Required for Emacs to find lein - WILL VARY BY HOST RUNNING Emacs!
(add-to-list 'exec-path "/usr/local/bin")

;; [Clojure mode]- https://github.com/clojure-emacs/clojure-mode
(unless (package-installed-p 'clojure-mode) (package-install 'clojure-mode))

;; [Clojure mode xtra font locking] - (keyword highlighting)
(unless (package-installed-p 'clojure-mode-extra-font-locking)
  (package-install 'clojure-mode-extra-font-locking))
(eval-after-load 'clojure-mode '(require 'clojure-mode-extra-font-locking))

;; [Editor intg for cats monad library] - http://funcool.github.io/cats/latest/#editor-integration
(require 'clojure-mode)
(define-clojure-indent
  (alet 'defun)
  (mlet 'defun))

;; [CIDER Debugger] - https://github.com/clojure-emacs/cider
(add-hook 'cider-mode-hook #'eldoc-mode)

;; WARNING: Makes repl autoreload. Boot projects are assumed to have boot-refresh
;;            https://github.com/samestep/boot-refresh
(setq cider-boot-parameters "repl -s watch refresh")
;;(setq cider-boot-parameters "dev")

;; [Clojure Refactor (CIDER based)] - https://github.com/clojure-emacs/clj-refactor.el
(unless (package-installed-p 'clj-refactor) (package-install 'clj-refactor))
(defun my-clojure-refactor-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my-clojure-refactor-hook)

;; [CIDER+Flycheck Clojure linting] - https://github.com/clojure-emacs/squiggly-clojure
;; (unless (package-installed-p 'flycheck-clojure) (package-install 'flycheck-clojure))
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (unless (package-installed-p 'flycheck-pos-tip) (package-install 'flycheck-pos-tip))
;; (eval-after-load 'flycheck '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; ************************************
;; ---~~~==== GENERAL LANG CONFIG  ====~~~---
;; ************************************
(unless (package-installed-p 'haskell-mode) (package-install 'haskell-mode))
(unless (package-installed-p 'groovy-mode) (package-install 'groovy-mode))

;; ************************************
;; ---~~~=====  DevOps CONFIG  =====~~~---
;; ************************************

;; [yaml-mode] - YAML syntax highlighting - https://github.com/yoshiki/yaml-mode
(unless (package-installed-p 'yaml-mode) (package-install 'yaml-mode))

;; [docker.el] - enables docker commands from Emacs - https://github.com/Silex/docker.el
;; try M-x docker-images
(unless (package-installed-p 'docker) (package-install 'docker))

;; OSX only Docker configuration, will probably break Emacs on Linux
;; (by default Linux installs docker to /bin/bash/docker, but OSX does not)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
;; Use "docker-machine env box" command to find out your environment variables
(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://10.11.12.13:2376")
(setenv "DOCKER_CERT_PATH" "/Users/foo/.docker/machine/machines/box")
(setenv "DOCKER_MACHINE_NAME" "box")

;; OSX only keybind config
(setq ns-function-modifier 'super)

;; [dockerfile-mode] - highlights for Dockerfiles - https://github.com/spotify/dockerfile-mode
(unless (package-installed-p 'dockerfile-mode) (package-install 'dockerfile-mode))

;; [google-this] - Google stuff under point - https://github.com/Malabarba/emacs-google-this
(unless (package-installed-p 'google-this) (package-install 'google-this))
(google-this-mode 1)

;; ************************************
;; ---~~~=====  VARs and FNs  ======~~~---
;; ************************************

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-repl-use-pretty-printing t)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (groovy-mode haskell-mode google-this yaml-mode dockerfile-mode docker visual-regexp-steroids clojure-mode-extra-font-locking aggressive-indent clj-refactor rainbow-delimiters clojure-mode)))
 '(safe-local-variable-values
   (quote
    ((eval when
	   (fboundp
	    (quote rainbow-mode))
	   (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;(load-theme 'solarized-dark)

;; causes all trailing whitespace to be removed
;; ref http://batsov.com/articles/2011/11/25/emacs-tip-number-3-whitespace-cleanup/
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Custom functions
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Refreshed open files."))

(defun format-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;; Sets the default text for the scratch buffer
(setq initial-scratch-message "=== GOAL ===\n\n=== PROBLEM STATEMENT ===\n\n=== Q&A ===\nWhy can't the goal be achieved?")

;; Below scratch FNs auto-reopen the *scratch* buffer when it's killed
;; This obviates the need to "figure out how to recreate the *scratch* buffer"
;; Stolen from: https://emacs.stackexchange.com/questions/20/re-open-scratch-buffer
(defun prepare-scratch-for-kill ()
  (save-excursion
    (set-buffer (get-buffer-create "*scratch*"))
    (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer t)))

(defun kill-scratch-buffer ()
  (let (kill-buffer-query-functions)
    (kill-buffer (current-buffer)))
  ;; no way, *scratch* shall live
  (prepare-scratch-for-kill)
  ;; Since we "killed" it, don't let caller try too
  nil)

(prepare-scratch-for-kill)

;; Navigation FNs
(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

;; ************************************
;; ---~~~=======  KEYBINDS  =======~~~---
;; ************************************

;; Visual regexp on steroids
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; Move to beginning of code or beginning of line (toggle)
(define-key global-map (kbd "C-a") 'smart-line-beginning)

;; Try to intelligently expand word at/before point
;; (global-set-key "\M- " 'hippie-expand)


;; Shamelessly stolen from Fuco1's smartparens setup
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
(bind-key "C-M-s"
          (defhydra smartparens-hydra ()
            "Smartparens"
            ("w" sp-down-sexp "Down")
            ("s" sp-up-sexp "Up")
            ("u" sp-backward-up-sexp "Up")
            ("a" sp-backward-down-sexp "Down")
            ("d" sp-forward-sexp "Forward")
            ("a" sp-backward-sexp "Backward")
            ("k" sp-kill-sexp "Kill" :color blue)
            ("q" nil "Quit" :color blue))
          smartparens-mode-map)

;; TODO, what are nicer keys for the above bindings, and how to set up/down/left/right to normal character movement?

;; From Matthew Emerson -- thanks!
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-o") #'cider-repl-clear-buffer)))

;; Sets prefix cmd for all "google-this" keys. Try `C-x g g`
(global-set-key (kbd "C-x g") 'google-this-mode-submap)
