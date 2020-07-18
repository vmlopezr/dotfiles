;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
(package! auto-dim-other-buffers)
(package! doom-modeline)
(package! flycheck)
(package! projectile)
(package! company-quickhelp)
(package! web-mode)
(package! dumb-jump)
(package! typescript-mode)
(package! tide)
(package! css-mode)
(package! ag)
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
(package! company-jedi)
(package! tramp)
(package! windsize)

;; disable packages
(package! neotree :disable t)
(package! magit :disable t)
(package! company-box :disable t)
(package! company-quickhelp :disable t)
