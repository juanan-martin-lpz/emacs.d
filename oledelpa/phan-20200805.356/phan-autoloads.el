;;; phan-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "phan" "phan.el" (0 0 0 0))
;;; Generated autoloads from phan.el

(autoload 'phan-log-mode "phan" "\
Major mode for viewing phan formatted log.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("/phan.*\\.log\\'" . phan-log-mode))

(autoload 'phan-find-config-file "phan" "\
Open Phan config file of the project." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "phan" '("phan-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; phan-autoloads.el ends here
