;;; init-docsets.el --- Dash docsets support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(global-set-key "\C-hp" 'helm-dash-at-point) ;; busca la documentación de la función señalada por el cursor
(global-set-key "\C-hs" 'helm-dash) ;; inicia el modo busqueda de helms dash

(setq helm-dash-common-docsets '("Express" ))


(provide 'init-docsets)
