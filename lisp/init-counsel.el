

(require-package 'counsel-dash)


(setq browse-url-browser-function 'browse-url-generic  browse-url-generic-program "vivaldi")

(setq counsel-dash-docsets-path "~/.docsets")
(setq counsel-dash-docsets-url "http://raw.github.com/Kapeli/feeds/master")
(setq counsel-dash-min-length 3)
(setq counsel-dash-candidate-format "%d %n (%t)")
(setq counsel-dash-enable-debugging nil)
(setq counsel-dash-browser-func 'browse-url)
(setq counsel-dash-ignored-docsets nil)


(provide 'init-counsel)
