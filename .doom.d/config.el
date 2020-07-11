(setq user-full-name "Victor Lopez"
      user-mail-address "vmlopez.r@gmail.com")
;; On start up set emacs to full screen
(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
  (toggle-frame-maximized)
  (toggle-frame-fullscreen))

;; change scroll behavior
(setq scroll-conservatively 100)
;; Move any customisations via the interface to .custom.el
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set defaults
(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

;; Set the font for emacs
(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 16)
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

;; Set ui defaults
(setq
 display-line-numbers-type 'relative  ;; set relative line numbers
 doom-theme 'doom-vibrant             ;;set emacs theme
 org-directory "~/org/"
 projectile-project-search-path '("~/Documents/GitHub/" "~/Documents/development"))

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
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

;; doom-modeline settings
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-height 30)
  (setq doom-modeline-bar-width 30)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (custom-set-faces!
    '(doom-modeline-buffer-modified :foreground "red"))
    '(doom-modeline-evil-insert-state :weight bold :foreground "#339CDB")

  :hook
  (after-init . doom-modeline-mode)
  )

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)
;; set auto-dim-other-buffers by default on start up
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))

;; configure company
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
(add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;;ESS
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))

;; Configure packages for js, ts and react
    (use-package flycheck
      :ensure t
      :config
      (add-hook 'typescript-mode-hook 'flycheck-mode))

    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      ;; company is an optional dependency. You have to
      ;; install it separately via package-install
      ;; `M-x package-install [ret] company`
      (company-mode +1))

    (use-package company
      :ensure t
      :config
      (setq company-show-numbers t)
      (setq company-tooltip-align-annotations t)
      ;; invert the navigation direction if the the completion popup-isearch-match
      ;; is displayed on top (happens near the bottom of windows)
      (setq company-tooltip-flip-when-above t)
      (global-company-mode))

    (use-package company-quickhelp
      :ensure t
      :init
      (company-quickhelp-mode 1)
      (use-package pos-tip
        :ensure t))

    (use-package web-mode
      :ensure t
      :mode (("\\.html?\\'" . web-mode)
             ("\\.tsx\\'" . web-mode)
             ("\\.jsx\\'" . web-mode))
      :config
      (setq web-mode-markup-indent-offset 2
            web-mode-css-indent-offset 2
            web-mode-code-indent-offset 2
            web-mode-block-padding 2
            web-mode-comment-style 2

            web-mode-enable-css-colorization t
            web-mode-enable-auto-pairing t
            web-mode-enable-comment-keywords t
            web-mode-enable-current-element-highlight t
    	web-mode-enable-auto-indentation nil
            )
      (add-hook 'web-mode-hook
                (lambda ()
                  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    		(setup-tide-mode))))
      ;; enable typescript-tslint checker
      (flycheck-add-mode 'typescript-tslint 'web-mode))

    (use-package typescript-mode
      :ensure t
      :config
      (setq typescript-indent-level 2)
      (add-hook 'typescript-mode #'subword-mode))

    (use-package tide
      :init
      :ensure t
      :after (typescript-mode company flycheck)
      :hook ((typescript-mode . tide-setup)
             (typescript-mode . tide-hl-identifier-mode)))

    (use-package css-mode
      :config (setq css-indent-offset 2))
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
 '(package-selected-packages (quote (org-plus-contrib))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
