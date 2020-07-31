;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
(package! doom-modeline)
(package! flycheck)
(package! projectile)
(package! dumb-jump)
(package! ag)
(package! editorconfig)
(package! rainbow-mode)
;; web-design packages
(package! typescript-mode)
(package! tide)
(package! prettier-js)
(package! web-mode)
(package! js2-mode)
(package! css-mode)
(package! scss-mode)
(package! json-mode)
(package! fill-column-indicator)
;;org packages
(package! doct
  :recipe (:host github :repo "progfolio/doct")
  :pin "9be788f9e3...")
(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "88380f865a...")
(package! org-pretty-tags :pin "40fd72f3e7...")
(package! ox-gfm :pin "99f93011b0...")
(package! org-ref :pin "b05d6b4434...")
(package! elpy)
;; Company packages
(package! company-jedi)
(package! tramp)
(package! windsize)
;; disable packages
(package! neotree :disable t)
(package! magit :disable t)
(package! tramp :disable t)
