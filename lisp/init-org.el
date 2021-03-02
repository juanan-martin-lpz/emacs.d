;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

(when *is-a-mac*
  (maybe-require-package 'grab-mac-link))

(require-package 'org)
(require-package 'org-bullets)

(maybe-require-package 'org-cliplink)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-jump-to-current-clock)
(define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
(define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)

(define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)


;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-enforce-todo-checkbox-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-tags-column 80)


;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))




(maybe-require-package 'writeroom-mode)

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (setq-local blink-cursor-interval 0.6)
        (setq-local show-trailing-whitespace nil)
        (setq-local line-spacing 0.2)
        (setq-local electric-pair-mode nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)


(setq org-support-shift-select t)

;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))



;;; Refiling

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

;; (setq org-todo-keywords
;;       (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
;;               (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
;;               (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
;;       org-todo-repeat-to-state "NEXT")

;; (setq org-todo-keyword-faces
;;       (quote (("NEXT" :inherit warning)
;;               ("PROJECT" :inherit font-lock-string-face))))



;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


;; (let ((active-project-match "-INBOX/PROJECT"))

;;   (setq org-stuck-projects
;;         `(,active-project-match ("NEXT")))

;;   (setq org-agenda-compact-blocks t
;;         org-agenda-sticky t
;;         org-agenda-start-on-weekday nil
;;         org-agenda-span 'day
;;         org-agenda-include-diary nil
;;         org-agenda-sorting-strategy
;;         '((agenda habit-down time-up user-defined-up effort-up category-keep)
;;           (todo category-up effort-up)
;;           (tags category-up effort-up)
;;           (search category-up))
;;         org-agenda-window-setup 'current-window
;;         org-agenda-custom-commands
;;         `(("N" "Notes" tags "NOTE"
;;            ((org-agenda-overriding-header "Notes")
;;             (org-tags-match-list-sublevels t)))
;;           ("g" "GTD"
;;            ((agenda "" nil)
;;             (tags "INBOX"
;;                   ((org-agenda-overriding-header "Inbox")
;;                    (org-tags-match-list-sublevels nil)))
;;             (stuck ""
;;                    ((org-agenda-overriding-header "Stuck Projects")
;;                     (org-agenda-tags-todo-honor-ignore-options t)
;;                     (org-tags-match-list-sublevels t)
;;                     (org-agenda-todo-ignore-scheduled 'future)))
;;             (tags-todo "-INBOX"
;;                        ((org-agenda-overriding-header "Next Actions")
;;                         (org-agenda-tags-todo-honor-ignore-options t)
;;                         (org-agenda-todo-ignore-scheduled 'future)
;;                         (org-agenda-skip-function
;;                          '(lambda ()
;;                             (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
;;                                 (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
;;                         (org-tags-match-list-sublevels t)
;;                         (org-agenda-sorting-strategy
;;                          '(todo-state-down effort-up category-keep))))
;;             (tags-todo ,active-project-match
;;                        ((org-agenda-overriding-header "Projects")
;;                         (org-tags-match-list-sublevels t)
;;                         (org-agenda-sorting-strategy
;;                          '(category-keep))))
;;             (tags-todo "-INBOX/-NEXT"
;;                        ((org-agenda-overriding-header "Orphaned Tasks")
;;                         (org-agenda-tags-todo-honor-ignore-options t)
;;                         (org-agenda-todo-ignore-scheduled 'future)
;;                         (org-agenda-skip-function
;;                          '(lambda ()
;;                             (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
;;                                 (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
;;                         (org-tags-match-list-sublevels t)
;;                         (org-agenda-sorting-strategy
;;                          '(category-keep))))
;;             (tags-todo "/WAITING"
;;                        ((org-agenda-overriding-header "Waiting")
;;                         (org-agenda-tags-todo-honor-ignore-options t)
;;                         (org-agenda-todo-ignore-scheduled 'future)
;;                         (org-agenda-sorting-strategy
;;                          '(category-keep))))
;;             (tags-todo "/DELEGATED"
;;                        ((org-agenda-overriding-header "Delegated")
;;                         (org-agenda-tags-todo-honor-ignore-options t)
;;                         (org-agenda-todo-ignore-scheduled 'future)
;;                         (org-agenda-sorting-strategy
;;                          '(category-keep))))
;;             (tags-todo "-INBOX"
;;                        ((org-agenda-overriding-header "On Hold")
;;                         (org-agenda-skip-function
;;                          '(lambda ()
;;                             (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
;;                                 (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
;;                         (org-tags-match-list-sublevels nil)
;;                         (org-agenda-sorting-strategy
;;                          '(category-keep))))
;;             ;; (tags-todo "-NEXT"
;;             ;;            ((org-agenda-overriding-header "All other TODOs")
;;             ;;             (org-match-list-sublevels t)))
;;             )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)


;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))



;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(with-eval-after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))



(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                "tell application \"org-clock-statusbar\" to clock out"))))



;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!



;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")





(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . t)
     (sqlite . t))))



(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  ;;:pin org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/Dropbox/Aplicaciones/MobileOrg/Tasks.org"
          "~/Dropbox/Aplicaciones/MobileOrg/Birthdays.org"))

  (setq org-directory "~/OrgFiles")
  (setq org-default-notes-file '("~/OrgFiles/notes.org'"))

  (setq org-mobile-inbox-for-pull "~/OrgFiles/flagged.org")
  (setq org-mobile-directory "~/Dropbox/Aplicaciones/MobileOrg")

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;;
  ;; Workflow
  ;; Gestion
  ;; PLAN   : Tarea a planificar             ;; TODO   : Tarea a realizar en un futuro cercano. Con fecha o sin ella
  ;; FUTURE : Tarea a realizar en un futuro indeterminado

  ;; Proyectos
  ;; PROJECT: Proyecto. DONE Cuando todas sus tareas esten DONE.

  ;; Gestion de Proyectos
  ;; ACTIVE : Tarea Activa
  ;; READY  : Tareas lista
  ;; WAIT   : Tarea en espera
  ;; HOLD   : Tarea retenida por algun motivo
  ;; BACKLOG: Tarea en el BACKLOG
  ;; REVIEW : Tarea para revisar

  (setq org-todo-keywords
        '((sequence "PROJECT(p)" "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CANCELLED(q@/!)" "PHONE" "MEETING")
          (sequence "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@/!)")))



  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t) ("ARCHIVE" . t))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD") ("REVIEW") ("ARCHIVE") ("NEXT" . t) ("REFILE" . t))
                ("HOLD" ("NEXT") ("WAITING") ("HOLD" . t) ("REFILE" . t))
                ("READY" ("NEXT") ("WAITING") ("HOLD") ("REFILE") ("REVIEW" . t))
                ("REVIEW" ("REVIEW") ("WAITING . t"))
                ("WAIT" ("WAITING" . t))
                ;;(done ("WAITING") ("HOLD") ("REVIEW"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD") ("REVIEW") ("REFILE") ("ARCHIVE") ("NEXT"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD") ("REVIEW") ("NEXT") ("ARCHIVE" . t)))))

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 30)))

            (todo "TODO"
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                   (org-agenda-overriding-header "Sin Fecha")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "A continuacion")))

            (tags-todo "REFILE"
                       ((org-agenda-overriding-header "Refile")))
            (tags-todo "WAITING"
                       ((org-agenda-overriding-header "En Espera")))
            (tags-todo "NEXT"
                       ((org-agenda-overriding-header "Siguientes")))
            (tags-todo "HOLD"
                       ((org-agenda-overriding-header "Retenidas")))
            (tags-todo "REVIEW"
                       ((org-agenda-overriding-header "En Revision")))
            (tags-todo "CANCELLED"
                       ((org-agenda-overriding-header "Canceladas")))))

          ("n" "Proximas Tareas"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Proximas Tareas")))))

          ("w" "Flujo de Trabajo"
           ((todo "PROJECT"
                  ((org-agenda-overriding-header "Proyectos")
                   (org-agenda-files org-agenda-files)))
            (todo "WAIT"
                  ((org-agenda-overriding-header "En espera" )
                   (org-agenda-files org-agenda-files)))
            (todo "HOLD"
                  ((org-agenda-overriding-header "Retenidas")
                   (org-agenda-files org-agenda-files)))
            (todo "PHONE"
                  ((org-agenda-overriding-header "Llamadas de Telefono")
                   (org-agenda-files org-agenda-files)))
            (todo "MEETING"
                  ((org-agenda-overriding-header "Reuniones/Entrevistas" )
                   (org-agenda-files org-agenda-files)))
            (todo "WAIT"
                  ((org-agenda-overriding-header )
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "Revisando")
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Listos para revisar")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Tareas Activas")
                   (org-agenda-files org-agenda-files)))
            (todo "HOLD"
                  ((org-agenda-overriding-header "Tareas en Espera")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Tareas Completadas")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Tareas Canceladas")
                   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/OrgFiles/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text nil)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))



(provide 'init-org)
;;; init-org.el ends here
