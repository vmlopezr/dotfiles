(setq user-full-name "Victor Lopez"
      user-mail-address "vmlopez.r@gmail.com")

;; Set the font for emacs
(setq
 doom-font (font-spec :family "monospace" :size 12 :weight 'bold)
 display-line-numbers-type 'relative  ;; set relative line numbers
 doom-theme 'doom-vibrant             ;;set emacs theme
 org-directory "~/org/"
 projectile-project-search-path '("~/Documents/GitHub/" "~/Documents/development")
  )

;; Set theme
(after! doom-themes
  (setq doom-neotree-file-icons t))

;; neotree settings
(after! neotree
  (setq doom-themes-neotree-file-icons 'icons)
  (setq doom-themes-neotree-enable-file-icons 'icons)
  (setq neo-theme 'icons))

;; projectile settings
(after! projectile
  (projectile-mode)
  (projectile-load-known-projects))

;; doom-modeline settings
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-height 30)
  (setq doom-modeline-bar-width 30)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (custom-set-faces
   '(mode-line ((t (:box (:line-width 2 :color "red"))))))
  (set-face-attribute 'mode-line nil
                    :background "#353644"
                    :foreground "red"
                    :box '(:line-width 8 :color "red")
                    :overline nil
                    :underline nil)

  (set-face-attribute 'mode-line-inactive nil
                    :background "#565063"
                    :foreground "white"
                    :box '(:line-width 8 :color "#565063")
                    :overline nil
                    :underline nil))

;; set auto-dim-other-buffers by default on start up
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))


;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (projectile org-plus-contrib neotree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:box (:line-width 2 :color "red"))))))
