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

;; TODO: fix colors being off in from `ansi-term` command

;; Makes Emacs remember open buffers between runs
(desktop-save-mode 1)

;; Disable logging messages when font-locking to speed up rendering
(setq font-lock-verbose nil)

;; Ido mode -- enables automatic matching for buffers and files as you type
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Vertical display of Ido mode - https://github.com/creichert/ido-vertical-mode.el
(unless (package-installed-p 'ido-vertical-mode)
  (package-install 'ido-vertical-mode))
(ido-mode 1)
(ido-vertical-mode 1)

;; Show Ido autocomplete options at point - https://github.com/katspaugh/ido-at-point
(unless (package-installed-p 'ido-at-point)
  (package-install 'ido-at-point))

;; Required for Emacs to find lein
(add-to-list 'exec-path "/usr/local/bin")

;; Clojure mode - https://github.com/clojure-emacs/clojure-mode
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

;; Editor intg for cats monad library - http://funcool.github.io/cats/latest/#editor-integration
(require 'clojure-mode)
(define-clojure-indent
  (alet 'defun)
  (mlet 'defun))

;; Extra font locking (keyword highlighting) for Clojure/CLJS
(unless (package-installed-p 'clojure-mode-extra-font-locking)
  (package-install 'clojure-mode-extra-font-locking))
(eval-after-load 'clojure-mode '(require 'clojure-mode-extra-font-locking))

;; Rainbow Delimiters - https://www.emacswiki.org/emacs/RainbowDelimiters
(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Aggressive Indent Mode - https://github.com/Malabarba/aggressive-indent-mode
(unless (package-installed-p 'aggressive-indent)
  (package-install 'aggressive-indent))
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; Paredit for major LISP modes - https://www.emacswiki.org/emacs/ParEdit
(unless (package-installed-p 'paredit)
  (package-install 'paredit))
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Allows use of modern regex - https://github.com/benma/visual-regexp-steroids.el
(unless (package-installed-p 'visual-regexp-steroids)
  (package-install 'visual-regexp-steroids))

;; CIDER - https://github.com/clojure-emacs/cider
(unless (package-installed-p 'cider)
  (package-install 'cider))
(add-hook 'cider-mode-hook #'eldoc-mode)

;; Solarized-theme - https://github.com/bbatsov/solarized-emacs
(unless (package-installed-p 'solarized-theme)
  (package-install 'solarized-theme))

;; Desktop+ allows saving/loading sets of files into buffers - https://github.com/ffevotte/desktop-plus
(unless (package-installed-p 'desktop+)
  (package-install 'desktop+))

;; Projectile provides first class abstractions for manipulating project files - https://github.com/bbatsov/projectile
(unless (package-installed-p 'projectile)
  (package-isntall 'projectile))

;; yaml-mode to provide YAML syntax support - https://github.com/yoshiki/yaml-mode
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

;; docker.el - https://github.com/Silex/docker.el
;; enables docker commands from Emacs
;; try M-x docker-images
(unless (package-installed-p 'docker)
  (package-install 'docker))

;; OSX only Docker configuration, will probably break Emacs on Linux
;; (by default Linux installs docker to /bin/bash/docker, but OSX does not)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
;; Use "docker-machine env box" command to find out your environment variables
(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://10.11.12.13:2376")
(setenv "DOCKER_CERT_PATH" "/Users/foo/.docker/machine/machines/box")
(setenv "DOCKER_MACHINE_NAME" "box")

;; syntax highlighting for Dockerfiles
(unless (package-installed-p 'dockerfile-mode)
  (package-install 'dockerfile-mode))

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
    (yaml-mode dockerfile-mode docker ido-at-point-mode ido-vertical-mode visual-regexp-steroids clojure-mode-extra-font-locking aggressive-indent clj-refactor solarized-theme rainbow-delimiters clojure-mode))))
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

;; Keybinds

;; Visual regexp on steroids
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
