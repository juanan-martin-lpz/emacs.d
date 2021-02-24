

(projectile-register-project-type 'ng '("angular.json" "package.json")
                  :project-file "angular.json"  
				  :compile "ng build"
				  :test "ng test"
				  :run "ng serve"
                  :src-dir "src"
				  :test-suffix ".spec")

(projectile-register-project-type 'npm '("package.json")
                  :project-file "package.json"
				  :compile "npm install"
				  :test "npm test"
				  :run "npm start"
				  :test-suffix ".spec")


(provide 'init-npmproj)