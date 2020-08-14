;; straight.el bootstrap -- https://github.com/raxod502/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; *********************************************
;; ---~~~====  SUPPORTING LIBRARIES  =====~~~---
;; *********************************************
;; A modern list library -- https://github.com/magnars/dash.el
;; REQUIRED FOR SOLARIZED THEME
(straight-use-package 'dash)

;; ***************************************
;; ---~~~====  GENERAL CONFIG  =====~~~---
;; ***************************************

;; Tells emacs to be generous when printing expressions (for instance, to the *Message* buffer)
(setq eval-expression-print-length nil)

;; BUG NOTE: You MUST manually create /emacs-backup/ or Projectile seems to stop working
;; flat file backups in one place. eliminates annoying files~ in git tree
;;(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "emacs-backup"))))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "emacs-saves") t)))

(setq create-lockfiles nil)

;; OSX: Resize to default MacBook Pro 2015 screensize
(setq initial-frame-alist
      `((left . 60) (top . 0)
	(width . 80) (height . 56)))

;; OSX: Disable Emacs GUI & correct resizing
(setq frame-resize-pixelwise t)

(setq ns-auto-hide-menu-bar t)

;; From David Ongaro -- thanks!
(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "C-x m") 'toggle-frame-fullscreen)

(defun enable-window-garbage ()
  (interactive)
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (scroll-bar-mode 1))

(defun disable-window-garbage ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(add-hook 'desktop-after-read-hook (lambda ()
				     (disable-window-garbage)))

(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq ns-pop-up-frames nil)

;; Tell Emacs to only GC when 80mb of garbage is reached
;; this prevents aggressive GCs that trigger several time a second and create bad UX
;; (If the current year is >=2022, you should probably increase this)
(setq gc-cons-threshold 80000000)

;; Removes default C-x c keybinding that closes Emacs
(global-unset-key (kbd "C-x C-c"))

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

(defun browse-url-chrome-workaround (url &rest ignore)
  "Uses custom shell script which opens chrome & sets url via AppleScript.
   Allows non http(s) URIs to be openable"
  (interactive "sURL: ")
  (shell-command (concat user-emacs-directory "/open-chrome-osx.sh " url)))
;; TODO: could never get M-x org-open-at-point to work on URLs like slack://
(setq browse-url-browser-function 'browse-url-chrome-workaround)

;; [org mode] - plaintext system for organization - https://orgmode.org/
;; NOTE: must have sqlite3 installed & on path
;;       test with (executable-find "sqlite3")
(straight-use-package 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-directory "~/org")
(setq org-agenda-files (list "~/org/concur.org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-todo-keywords
      '((sequence
	 "WISH"
	 "TODO"
	 "WAITING"
	 "BLOCKED"
	 "|"
	 "DONE")))

;; [org-babel] - eval code snippets in orgmode - https://orgmode.org/worg/org-contrib/babel
(straight-use-package 'org-babel-eval-in-repl)

;; [ob-http] - http request in org-mode babel - https://github.com/zweifisch/ob-http
(straight-use-package 'ob-http)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (http . t)))

;; [org-roam] - hierarchical wiki of notes in orgmode - https://github.com/org-roam/org-roam
(straight-use-package 'org-roam)
(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-directory "~/org/roam")
;; org-mode-roam-map - should technically only be active in this mode and not global
(global-set-key (kbd "<C-c n l>") 'org-roam)
(global-set-key (kbd "<C-c n f>") 'org-roam-find-file)
(global-set-key (kbd "<C-c n g>") 'org-roam-graph-show)
;; org-mode-map
(global-set-key (kbd "<C-c n i>") 'org-roam-insert)
(global-set-key (kbd "<C-c n I>") 'org-roam-insert-immediate)

;; emacs-libvterm - fully developed bash terminal - https://github.com/akermu/emacs-libvterm
(straight-use-package 'vterm)
(setq vterm-shell (executable-find "bash"))


;; [buffer-move] - transpose buffers - https://github.com/lukhas/buffer-move
(straight-use-package 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; [sublimity] - smooth scrolling - https://github.com/zk-phi/sublimity
(straight-use-package 'sublimity)
(require 'sublimity-scroll)
(setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 5)
(define-globalized-minor-mode my-sublimity-mode sublimity-mode
  (lambda () (sublimity-mode 1)))
(my-sublimity-mode 1)

;; (require 'sublimity-map)
;; (setq sublimity-map-size 18)
;; (setq sublimity-map-fraction 0.3)
;; (setq sublimity-map-text-scale -7)
;; (add-hook 'sublimity-map-setup-hook
;;           (lambda ()
;;             (setq buffer-face-mode-face '(:family "Monospace"))
;;             (buffer-face-mode)))
;; (sublimity-map-set-delay 8)

;; [Helm] - incremental completion and selection narrowing - https://github.com/emacs-helm/helm
(straight-use-package 'helm)
(require 'helm-config)
(helm-mode 1)

;; Stolen from Tuhdo's config -- http://tuhdo.github.io/helm-intro.html -- thanks!
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 30)
(helm-autoresize-mode 1)

(define-key helm-map (kbd "<left>") 'helm-previous-source)
(define-key helm-map (kbd "<right>") 'helm-next-source)

(customize-set-variable 'helm-ff-lynx-style-map t)

;; Replace some built-in FNs with the helm upgrades
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; [company-mode] - in-buffer completion dropdown - http://company-mode.github.io/
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; [Rainbow Delimiters] - multicolor parens - https://www.emacswiki.org/emacs/RainbowDelimiters
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; [Rainbow Mode] - color-code hexstrings - https://github.com/emacsmirror/rainbow-mode
(straight-use-package 'rainbow-mode)

;; [Solarized-theme] - https://github.com/bbatsov/solarized-emacs
;;(straight-use-package 'solarized-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq solarized-distinct-doc-face t)

;; [Desktop+] - save/load sets of files into buffers - https://github.com/ffevotte/desktop-plus
(straight-use-package 'desktop+)

;; [Projectile] - 1st class abstractions on project files - https://github.com/bbatsov/projectile
(straight-use-package 'projectile)
(projectile-global-mode) ;; enabled for all modes

;; Required workaround for helm-projectile (below)
;; see https://github.com/bbatsov/helm-projectile/issues/116
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; [Helm-Projectile] - Helm completion in Projectile commands - https://github.com/bbatsov/helm-projectile
(straight-use-package 'helm-projectile)
(require 'helm-projectile)
(helm-projectile-on)

;; [Aggressive Indent Mode] - https://github.com/Malabarba/aggressive-indent-mode
(straight-use-package 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; [Modern Paredit for major modes] - https://github.com/Fuco1/smartparens
;;  - has support for Paredit in non-LISP modes, but not yet understood/enabled
(straight-use-package 'smartparens)
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
(straight-use-package 'hydra)

;; [Modern Regex] - Provides modern regex cmds - www.github.com/benma/visual-regexp-steroids.el
(straight-use-package 'visual-regexp-steroids)

;; [ggtags] - improves Emacs symbol tagging - https://github.com/leoliu/ggtags
(straight-use-package 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'clojure-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; [magit] - Manipulate Git repos from Emacs - https://github.com/vermiculus/magithub
(straight-use-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; [magit-popup] - Generic wrapping interface over Emacs commands - https://github.com/magit/magit-popup
;;                 I don't use it, but is dependency of magit & was causing errors. See below
(straight-use-package 'magit-popup)
;; NOTE: M-x magit-branch-popup causes below error unless explicit loaded
;;       Unknown if this is due to a bug in straight.el loading or some caching mechanism
;;  ERR: helm-M-x: Wrong type argument: magit-popup-event, [cl-struct-magit-popup-event 100 "branch.%s.description" magit-format-branch*description magit-edit-branch*description nil nil]
;; This defers eval until first use of magit, which fixes error at cost of small 1-time delay
(with-eval-after-load 'magit-files
  (straight-rebuild-package "magit-popup"))

;; [magithub] - Browse GitHub - https://github.com/vermiculus/magithub
(straight-use-package 'magithub)

;; **************************************
;; ---~~~====  CLOJURE CONFIG  ====~~~---
;; **************************************

;; [lein config] - Required for Emacs to find lein - WILL VARY BY HOST RUNNING Emacs!
;; You may need to symlink lein to /usr/local/bin in order for some Clj tooling to work
(add-to-list 'exec-path "/Users/quest/bin")
(add-to-list 'exec-path "/Applications/clojure")

;; [Clojure mode]- https://github.com/clojure-emacs/clojure-mode
(straight-use-package 'clojure-mode)

;; [Clojure mode xtra font locking] - (keyword highlighting)
(straight-use-package 'clojure-mode-extra-font-locking)
(eval-after-load 'clojure-mode '(require 'clojure-mode-extra-font-locking))

;; [Editor intg for cats monad library] - http://funcool.github.io/cats/latest/#editor-integration
(require 'clojure-mode)
(define-clojure-indent
  (alet 'defun)
  (mlet 'defun))

;; [CIDER Debugger] - https://github.com/clojure-emacs/cider
(straight-use-package 'cider)
(add-hook 'cider-mode-hook #'eldoc-mode)

(setq cider-default-repl-command "boot")

;; disabled, but can eval to jack-in with specific options in lein invocation
;;(setq cider-lein-global-options "with-profiles local nrepl")

;; WARNING: Makes repl autoreload. Boot projects are assumed to have boot-refresh
;;            https://github.com/samestep/boot-refresh
(setq cider-boot-parameters "repl -s watch refresh")
;;(setq cider-boot-parameters "dev")

;; TODO: can set this to value `fipp` or `puget`, ref https://goo.gl/Pu4UQc
;;(setq cider-pprint-fn "user/my-pprint")

;; [Clojure Refactor (CIDER based)] - https://github.com/clojure-emacs/clj-refactor.el
;; (straight-use-package 'clj-refactor)
;; (defun my-clojure-refactor-hook ()
;;   (clj-refactor-mode 1)
;;   (yas-minor-mode 1) ; for adding require/use/import statements
;;   ;; This choice of keybinding leaves cider-macroexpand-1 unbound
;;   (cljr-add-keybindings-with-prefix "C-c C-m"))
;; (add-hook 'clojure-mode-hook #'my-clojure-refactor-hook)

;; [CIDER+Flycheck Clojure linting] - https://github.com/clojure-emacs/squiggly-clojure
;; disabled 2019/07/25 due to constantly eval'ing namespaces and causing measurable UI slowdown
;;(straight-use-package 'flycheck-clojure)
;;(eval-after-load 'flycheck '(flycheck-clojure-setup))
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;;(straight-use-package 'flycheck-pos-tip)
;;(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
;; (eval-after-load 'flycheck
;;   '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; Syntax highlighting on ~/.closhrc - https://github.com/dundalek/closh
(add-to-list 'auto-mode-alist '(".closhrc\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("profiles.boot\\'" . clojure-mode))

;; ******************************************
;; ---~~~==== GENERAL LANG CONFIG  ====~~~---
;; ******************************************
(straight-use-package 'haskell-mode)
(straight-use-package 'groovy-mode)

;; ***************************************
;; ---~~~=====  DevOps CONFIG  =====~~~---
;; ***************************************

;; [yaml-mode] - YAML syntax highlighting - https://github.com/yoshiki/yaml-mode
(straight-use-package 'yaml-mode)

;; [docker.el] - enables docker commands from Emacs - https://github.com/Silex/docker.el
;; try M-x docker-images
(straight-use-package 'docker)

;; OSX only Docker configuration, will probably break Emacs on Linux
;; (by default Linux installs docker to /bin/bash/docker, but OSX does not)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
;; Use "docker-machine env box" command to find out your environment variables
(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://10.11.12.13:2376")
(setenv "DOCKER_CERT_PATH" "/Users/quest/.docker/machine/machines/box")
(setenv "DOCKER_MACHINE_NAME" "box")

;; OSX only keybind config
(setq ns-function-modifier 'super)

;; [dockerfile-mode] - highlights for Dockerfiles - https://github.com/spotify/dockerfile-mode
(straight-use-package 'dockerfile-mode)

;; [google-this] - Google stuff under point - https://github.com/Malabarba/emacs-google-this
(straight-use-package 'google-this)
(google-this-mode 1)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-repl-use-pretty-printing t)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(helm-ff-lynx-style-map t)
 '(package-selected-packages
   (quote
    (keyfreq groovy-mode haskell-mode google-this yaml-mode dockerfile-mode docker visual-regexp-steroids clojure-mode-extra-font-locking aggressive-indent clj-refactor rainbow-delimiters clojure-mode)))
 '(safe-local-variable-values
   (quote
    ((cider-default-cljs-repl . shadow)
     (cider-shadow-cljs-default-options . "app")
     (eval when
	   (fboundp
	    (quote rainbow-mode))
	   (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Menlo")))))

(load-theme 'solarized-dark)

;; causes all trailing whitespace to be removed
;; ref http://batsov.com/articles/2011/11/25/emacs-tip-number-3-whitespace-cleanup/
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; **************************************
;; ---~~~======= FUNCTIONS  =======~~~---
;; **************************************

(defun kill-all-buffers ()
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

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
  (message "Killed all buffers except current"))

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

;; Automatically switch to next buffer when opening a new buffer window (C-x 2/3)
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun resize-window (&optional arg)    ; Hirose Yuuji and Bob Wiener
  "*Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
	(message
	 "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, q=quit"
	 arg)
	(setq c (read-char))
	(condition-case ()
	    (cond
	     ((= c ?h) (enlarge-window arg))
	     ((= c ?s) (shrink-window arg))
	     ((= c ?w) (enlarge-window-horizontally arg))
	     ((= c ?n) (shrink-window-horizontally arg))
	     ((= c ?\^G) (keyboard-quit))
	     ((= c ?q) (throw 'done t))
	     ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
	     (t (beep)))
	  (error (beep)))))
    (message "Done.")))

;; `C-x o` moves window focus forward 1 -- this alias makes  capital O move back 1
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))
(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

;; insert an empty line after the current line and position the cursor on its beginning
(defun insert-empty-line ()
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(global-set-key [(shift return)] 'insert-empty-line)
(global-set-key [(control return)] 'other-window)
(global-set-key [(meta return)] (lambda () (interactive) (other-window -1)))

;; **************************************
;; ---~~~=======  KEYBINDS  =======~~~---
;; **************************************

;; Visual regexp on steroids
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "S-c q") 'vr/query-replace)

;; Move to beginning of code or beginning of line (toggle)
(define-key global-map (kbd "C-a") 'smart-line-beginning)

;; Try to intelligently expand word at/before point
;; (global-set-key "\M- " 'hippie-expand)


;; Shamelessly stolen from Fuco1's smartparens setup
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;; (bind-key "C-M-s"
;;           (defhydra smartparens-hydra ()
;;             "Smartparens"
;;             ("w" sp-down-sexp "Down")
;;             ("s" sp-up-sexp "Up")
;;             ("u" sp-backward-up-sexp "Up")
;;             ("a" sp-backward-down-sexp "Down")
;;             ("d" sp-forward-sexp "Forward")
;;             ("a" sp-backward-sexp "Backward")
;;             ("k" sp-kill-sexp "Kill" :color blue)
;;             ("q" nil "Quit" :color blue))
;;           smartparens-mode-map)

;; TODO, what are nicer keys for the above bindings, and how to set up/down/left/right to normal character movement?

;; From Matthew Emerson -- thanks!
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-o") #'cider-repl-clear-buffer)))

;; Sets prefix cmd for all "google-this" keys. Try `C-x g g`
(global-set-key (kbd "C-x g") 'google-this-mode-submap)

;; https://stackoverflow.com/questions/18316665/how-to-improve-emacs-performance-when-view-large-file
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 102400 20))
    ;;(setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (linum-mode -1)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)
