;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile))

(require 'projectile)

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)

(define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(projectile-register-project-type 'maven '("pom.xml")
                                  :project-file "pom.xml"
				  :compile "mvn compile"
				  :test "mvn test"
				  :run "mvn install"
				  :test-suffix ".spec")


(projectile-register-project-type 'composer '("composer.json")
                                  :project-file "composer.json"
				  :compile "composer install"
				  :test "composer test"
				  :run "composer serve"
				  :test-suffix ".spec")

(projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json"
				  :compile "npm install"
				  :test "npm test"
				  :run "npm start"
				  :test-suffix ".spec")

(projectile-register-project-type 'ng '("angular.json" "package.json" "tsconfig.json")
                                  :project-file "angular.json"
				  :compile "ng build"
				  :test "ng test"
				  :run "ng serve -o --watch=true --liveReload=true --port=6000"
                                  :src-dir "src"
				  :test-suffix ".spec")

(projectile-register-project-type 'spago '("spago.dhall")
                                  :project-file "spago.json"
				  :compile "spago build"
				  :test "spago test"
				  :run "spago run"
				  :test-suffix ".spec")


(defun projectile-create-projectile-project ()
  "Crea un proyecto projectile en la carpeta indicada."
  (interactive)
  (make-directory)
  (create-empty-file ".projectile")
  (magit-init)
  )

(define-key projectile-mode-map (kbd "C-c p n") 'projectile-create-projectile-project)

(setq projectile-globally-ignored-directories '("*node_modules" "*dist" "*.vscode" "*.stack-work" "*.git" "*spago" "bower_components" ))

(projectile-mode +1)

;; (require-package 'persp-mode)
;; (require-package 'nameframe-perspective)
;; (require-package 'nameframe-projectile)

;; (persp-mode)

;; (nameframe-projectile-mode 1)
;; (nameframe-perspective-mode 1)

;; (global-set-key (kbd "M-P") 'nameframe-switch-frame)

(global-set-key (kbd "<f3>") 'neotree-show)

(provide 'init-projectile)
;;; init-projectile.el ends here
