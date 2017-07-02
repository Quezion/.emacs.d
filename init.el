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
(package-refresh-contents) ;; Required to maintain updated package list

;; ************************************
;; ---~~~====  GENERAL CONFIG  =====~~~---
;; ************************************

;; Tell Emacs to only GC when 40mb of garbage is reached
;; this prevents aggressive GCs that trigger several time a second and create bad UX
;; (If the current year is >=2020, you should probably increase this.)
(setq gc-cons-threshold 40000000)

;; TODO: fix colors being off in from `ansi-term` command

;; Makes Emacs remember open buffers between runs
(desktop-save-mode 1)

;; Disable logging messages when font-locking to speed up rendering
(setq font-lock-verbose nil)

;; [Helm] - incremental completion and selection narrowing - https://github.com/emacs-helm/helm
(unless (package-installed-p 'helm) (package-install 'helm))
(require 'helm-config)
(helm-mode 1)
;; Replace some built-in FNs with the helm upgrades
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; [Flycheck] - inline error highlighting - http://www.flycheck.org/en/latest/
(unless (package-installed-p 'flycheck) (package-install 'flycheck))
(global-flycheck-mode)
;; OSX workaround for Flycheck - https://github.com/purcell/exec-path-from-shell
(unless (package-installed-p 'exec-path-from-shell) (package-install 'exec-path-from-shell))

;; [Rainbow Delimiters] - multicolor parens - https://www.emacswiki.org/emacs/RainbowDelimiters
(unless (package-installed-p 'rainbow-delimiters) (package-install 'rainbow-delimiters))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; [Solarized-theme] - https://github.com/bbatsov/solarized-emacs
(unless (package-installed-p 'solarized-theme) (package-install 'solarized-theme))

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

;; [Modern Paredit for major LISP modes] - https://github.com/Fuco1/smartparens
;;  - has support for Paredit in non-LISP modes, but not yet understood/enabled
(unless (package-installed-p 'smartparens) (package-install 'smartparens))
(require 'smartparens-config)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook       #'smartparens-strict-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-strict-mode)
(add-hook 'ielm-mode-hook             #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook             #'smartparens-strict-mode)
(add-hook 'lisp-interaction-mode-hook #'smartparens-strict-mode)
(add-hook 'scheme-mode-hook           #'smartparens-strict-mode)

;; [Modern Regex] - Provides modern regex cmds - https://github.com/benma/visual-regexp-steroids.el
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

;; [dockerfile-mode] - highlights for Dockerfiles - https://github.com/spotify/dockerfile-mode
(unless (package-installed-p 'dockerfile-mode) (package-install 'dockerfile-mode))


;; ************************************
;; ---~~~=====  VARs and FNs  ======~~~---
;; ************************************

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-repl-use-pretty-printing t)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(package-selected-packages
   (quote
    (yaml-mode dockerfile-mode docker visual-regexp-steroids clojure-mode-extra-font-locking aggressive-indent clj-refactor solarized-theme rainbow-delimiters clojure-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'solarized-dark)

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

;; Below scratch FNs auto-reopen the m*scratch* buffer when it's killed
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



;; ************************************
;; ---~~~=======  KEYBINDS  =======~~~---
;; ************************************

;; Visual regexp on steroids
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
