;; [[file:config.org::*Basic Configuration][Basic Configuration:1]]
(setq user-full-name "Victor Lopez"
      user-mail-address "vmlopez.r@gmail.com")
;; Basic Configuration:1 ends here

;; [[file:config.org::*Basic Configuration][Basic Configuration:2]]
(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
  (toggle-frame-maximized)
  (toggle-frame-fullscreen))
;; Basic Configuration:2 ends here

;; [[file:config.org::*Basic Configuration][Basic Configuration:3]]
;; Set defaults
(require 'windsize)
(windsize-default-keybindings)
(require 'fill-column-indicator)
(add-hook 'emacs-lisp-mode-hook (lambda () (fci-mode 1)))
(setq
 org-startup-folded t
 company-quickhelp-delay 0.3
 fci-rule-width 1
 fci-rule-color "darkblue"
 fci-rule-column 80
 windsize-cols 1
 windsize-rows 1
 pos-tip-border-width 3
 pos-tip-use-relative-coordinates 1
 undo-limit 80000000                         ; Raise undo-limit to 80Mb
 truncate-lines 0
 evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
 auto-save-default t                         ; Nobody likes to loose work, I certainly don't
 inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
 truncate-string-ellipsis "…"               ; Unicode ellispis are nicer than "...", and also save
 doom-font (font-spec :family "JetBrains Mono" :size 14)
 doom-variable-pitch-font (font-spec :family "Overpass" :size 16)
 doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light)
 display-line-numbers-type 'relative
 doom-theme 'doom-vibrant             ;;set emacs theme
 org-directory "~/org/"
 projectile-project-search-path '("~/Documents/GitHub/" "~/Documents/development"))
;; Basic Configuration:3 ends here

;; [[file:config.org::*Treemacs][Treemacs:1]]
(use-package treemacs
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq treemacs-show-hidden-files t))
(treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace)
(treemacs-define-RET-action 'file-node-open #'treemacs-visit-node-ace)
;; Treemacs:1 ends here

;; [[file:config.org::*Projectile][Projectile:1]]
(after! projectile
  (projectile-mode)
  (projectile-load-known-projects)
  (projectile-discover-projects-in-search-path))
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))
;; Projectile:1 ends here

;; [[file:config.org::*Doom-Modeline][Doom-Modeline:1]]
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
  (after-init . doom-modeline-mode))
;; Doom-Modeline:1 ends here

;; [[file:config.org::*Auto-Dim-Other-Buffers][Auto-Dim-Other-Buffers:1]]
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))
;; Auto-Dim-Other-Buffers:1 ends here

;; [[file:config.org::*Company][Company:1]]
;; (setq
;;  pos-tip-background-color "#cbcbcb"
;;  pos-tip-foreground-color "#444444"
;;  )
(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-begin-commands '(self-insert-command))
  (company-require-match 'never)
  (company-global-modes '(not shell-mode eaf-mode))
  (company-idle-delay 0.3)
  (company-show-numbers t)
  :config
  (custom-set-faces!
    '(company-tooltip :background "#354069" :foreground "#aed4d0")
    '(company-tooltip-common :foreground "#f53302")
    '(company-tooltip-selection :background "#98d4f5" :foreground "#294757")
    )
  (global-company-mode 1)
  (set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate)))

;; (use-package company-quickhelp
;;   :ensure t
;;   :init
;;   (company-quickhelp-mode 1)
;;   (use-package pos-tip
;;   :ensure t))
;; Company:1 ends here

;; [[file:config.org::*Flycheck][Flycheck:1]]
(use-package flycheck
    :ensure t
    :config
    ;;(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'typescript-mode-hook 'flycheck-mode)
    (add-hook 'elpy-mode-hook 'flycheck-mode)
)
;; Flycheck:1 ends here

;; [[file:config.org::*Typescript-Mode][Typescript-Mode:1]]
(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))
;; Typescript-Mode:1 ends here

;; [[file:config.org::*TIDE][TIDE:1]]
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
  (company-mode +1))
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(use-package tide
  :init
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))
;; TIDE:1 ends here

;; [[file:config.org::*Web-Mode][Web-Mode:1]]
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
    web-mode-enable-auto-indentation nil)
    (add-hook 'web-mode-hook
              (lambda ()
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (setup-tide-mode))))
    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode))
;; Web-Mode:1 ends here

;; [[file:config.org::*CSS-Mode][CSS-Mode:1]]
(use-package css-mode
  :config (setq css-indent-offset 2))
;; CSS-Mode:1 ends here

;; [[file:config.org::*Load Files][Load Files:1]]
(custom-set-variables
 '(package-selected-packages (quote (org-plus-contrib))))
(custom-set-faces)
(load! "bindings")
;; Load Files:1 ends here
