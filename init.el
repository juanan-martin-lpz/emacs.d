;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

(let ((minver "24.5"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------


;;----------------------------------------------------------------------------
;; straight package mamanger
;;----------------------------------------------------------------------------

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

;;
;;
;;

;;----------------------------------------------------------------------------
;; use-package
;;----------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require-package 'use-package)



(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-fontawesome)
(require 'init-themes)
(require 'init-winsuper)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-recentf)
(require 'init-smex)
(require 'init-hippie-expand)
(require 'init-company)
(require 'init-ivy)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(require 'init-compile)
(require 'init-crontab)
(require 'init-textile)
(require 'init-org)
(require 'init-http)

(require 'init-customs)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Cargamos el soporte para los lenguajes de programacion
;;----------------------------------------------------------------------------

(when 'markdown
  (require 'init-markdown))

(when 'csv
  (require 'init-csv))

(when 'erlang
  (require 'init-erlang))

(when 'javascript
  (require 'init-javascript))

(when 'typescript
  (require 'init-typescript))

(when 'php
  (require 'init-php))

(when 'nxml
  (require 'init-nxml))

(when 'html
  (require 'init-html))

(when 'css
  (require 'init-css))

(when 'haml
  (require 'init-haml))

(when 'python
  (require 'init-python))

(when 'haskell
  (require 'init-haskell))

(when 'elm
  (require 'init-elm))

(when 'purescript
  (require 'init-purescript))

(when 'ruby
  (require 'init-ruby))

(when 'rails
  (require 'init-rails))

(when 'elixir
  (require 'init-elixir))

(when 'sql
  (require 'init-sql))

(when 'nim
  (require 'init-nim))

(when 'rust
  (require 'init-rust))


(when (bound-and-true-p toml)
  (require 'init-toml))

(when 'yaml
  (require 'init-yaml))

(when 'docker
  (require 'init-docker))

(when 'terraform
  (require 'init-terraform))

(when 'nix
  (require 'init-nix))

(when 'clojure
  (progn
    (require 'init-clojure)
    (require 'init-clojure-cider)))

(when 'lisp
  (progn
    (require 'init-lisp)
    (require 'init-slime)
    (require 'init-common-lisp)
    ))

(maybe-require-package 'nginx-mode)

(require 'init-paredit)

(require 'init-yasnippet)


(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

;;(require 'init-yafolding)
(require 'init-folding)
(require 'init-dash)
(require 'init-counsel)

(require 'init-multiplecursors)

;; (require 'init-npmproj)
(require 'init-codesearch)
(require 'init-neotree)

(require 'init-docsets)

;;(require 'init-twitter)
;; (require 'init-mu)
(require 'init-ledger)

;; LSP
(require 'init-lsp)


;; Extra packages which don't require any configuration


(unless (package-installed-p 'esup)
  (package-refresh-contents)
  (package-install 'esup))

(require-package 'sudo-edit)
(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)

(when *is-a-mac*
  (require-package 'osx-location))
(unless (eq system-type 'windows-nt)
  (maybe-require-package 'daemons))
(maybe-require-package 'dotenv-mode)
(maybe-require-package 'shfmt)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(require 'init-direnv)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))


;;----------------------------------------------------------------------------
;; Funcion para ducplicar lineas
;;----------------------------------------------------------------------------


(defun duplicate-line-down()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(defun duplicate-line-up()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (prev-line 1)
  (yank)
  )

(define-prefix-command 'duplicate-line)
(global-set-key (kbd "C-d") 'duplicate-line)

(define-key duplicate-line (kbd "d") 'duplicate-line-down)
(define-key duplicate-line (kbd "u") 'duplicate-line-down)


;;-----------------------------------------------------------------------------------
;; add-auto-mode
;;-----------------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)





(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
