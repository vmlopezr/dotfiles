;; [[file:config.org::*Basic Configuration][Basic Configuration:1]]
(setq user-full-name "Victor Lopez"
      user-mail-address "vmlopez.r@gmail.com")
;; Basic Configuration:1 ends here

;; [[file:config.org::*Basic Configuration][Basic Configuration:2]]
;; Removes toolbar, scroll bar and menu bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq-default show-trailing-whitespace t)
;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))


(windsize-default-keybindings)
;;General hooks
(add-hook 'emacs-lisp-mode-hook (lambda () (fci-mode 1)))
(add-hook 'company-mode-hook (lambda () (fci-mode 1)))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; Set variables
(setq
 org-startup-folded t
 fci-rule-width 4
 fci-rule-column 80
 windsize-cols 5
 windsize-rows 5
 undo-limit 80000000                         ; Raise undo-limit to 80Mb
 truncate-lines 0
 evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
 auto-save-default t                         ; Nobody likes to loose work, I certainly don't
 inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
 doom-font (font-spec :family "JetBrains Mono" :size 14)
 doom-variable-pitch-font (font-spec :family "Overpass" :size 16)
 doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light)
 display-line-numbers-type 'relative
 doom-theme 'doom-tomorrow-night
 org-directory "~/org/")
;; Basic Configuration:2 ends here

;; [[file:config.org::*Doom Theme][Doom Theme:1]]
(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (require 'doom-themes-ext-org))
(setq
 evil-normal-state-cursor '(box "light blue")
 evil-insert-state-cursor '(bar "#ff0000")
 evil-visual-state-cursor '(hollow "orange")
 )
(custom-set-faces!
  '(hl-line :background "#444")
  '(region :background "#dbb672"))
;; Doom Theme:1 ends here

;; [[file:config.org::*Treemacs][Treemacs:1]]
(use-package treemacs
  :config
  (setq
    treemacs-width 30
    treemacs-wrap-around nil
    treemacs-silent-refresh t
    treemacs-no-png-images nil
    treemacs-silent-filewatch t
    treemacs-file-event-delay 1000
    treemacs-show-hidden-files t))
(treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace)
(treemacs-define-RET-action 'file-node-open #'treemacs-visit-node-ace)
;; Treemacs:1 ends here

;; [[file:config.org::*All The Icons][All The Icons:1]]
(use-package all-the-icons
  :ensure t
  :config
  (setq all-the-icons-color-icons t))
;; All The Icons:1 ends here

;; [[file:config.org::*Projectile][Projectile:1]]
(use-package projectile
  :diminish projectile-mode
  :commands (projectile-project-root)
  :config
  (projectile-mode)
  (setq
   projectile-sort-order 'recently-active
   projectile-project-search-path '("~/Documents/GitHub/" "~/Documents/development"))
  (projectile-load-known-projects)
  (projectile-discover-projects-in-search-path)
  (add-to-list 'projectile-globally-ignored-directories "~/.emacs.d")
  (add-to-list 'projectile-globally-ignored-directories "*node_modules"))
;; (setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/" "node_modules"))
;; (defun projectile-ignored-project-function (filepath)
;;   "Return t if FILEPATH is within any of `projectile-ignored-projects'"
;;   (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))
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
    '(doom-modeline-evil-insert-state :weight bold :foreground "#4ff0af")
  :hook
  (after-init . doom-modeline-mode))
;; Doom-Modeline:1 ends here

;; [[file:config.org::*Auto-Dim-Other-Buffers][Auto-Dim-Other-Buffers:1]]
;; (add-hook 'after-init-hook (lambda ()
;;   (when (fboundp 'auto-dim-other-buffers-mode)
;;     (auto-dim-other-buffers-mode t))))
;; Auto-Dim-Other-Buffers:1 ends here

;; [[file:config.org::*Company][Company:1]]
(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-begin-commands '(self-insert-command))
  (company-require-match 'never)
  (company-global-modes '(not shell-mode eaf-mode))
  (company-idle-delay 0.05)
  :config
  (custom-set-faces!
    '(company-tooltip :background "#445263" :foreground "#63a68b")
    '(company-tooltip-common :foreground "#f53302")
    '(company-tooltip-selection :background "#98d4f5" :foreground "#294757")
    '(company-preview-common :foreground "#aed4d0")
    '(company-tooltip-annotation :foreground "#aed4d0")
    '(company-tooltip-annotation-selection :foreground "#aed4d0")
    )
  )

;; (use-package company-quickhelp
;;   :ensure t
;;   :init
;;   (company-quickhelp-mode 1)
;;   (use-package pos-tip
;;   :ensure t))
;; Company:1 ends here

;; [[file:config.org::*Flycheck][Flycheck:1]]
(use-package flycheck
  :init
  (add-hook 'js2-mode-hook #'eslint-node-modules)
  (add-hook 'typescript-mode-hook #'eslint-node-modules)
  :config
  (flymake-mode-off)
  (global-flycheck-mode)
  (evil-define-key 'normal flycheck-mode-map
    (kbd "gh") 'flycheck-display-error-at-point)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  ;; (add-hook 'typescript-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
)
;; Flycheck:1 ends here

;; [[file:config.org::*Utils][Utils:1]]
;; Set local eslint path
(defun my-web-mode-hook ())
(defun eslint-node-modules ()
  (let* ((root (locate-dominating-file (or (buffer-file-name) default-directory) "node_modules"))
         (eslint (and root (expand-file-name "node_modules/.bin/eslint" root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
;; Set up tide mode
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (set (make-local-variable 'company-backends)
       '((company-tide company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev)))
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append))
;; Utils:1 ends here

;; [[file:config.org::*JSON][JSON:1]]
(use-package json-mode
  :mode "\\.json$")
(add-to-list 'flycheck-disabled-checkers 'json-python-json)

(use-package js2-mode
  :mode "\\.js\\'"
  :diminish js2-mode
  :config
  (setq js2-basic-offset 2)
  (setq js-indent-level 2))
;; JSON:1 ends here

;; [[file:config.org::*Prettier][Prettier:1]]
(use-package prettier-js
  :defer t)
;; Prettier:1 ends here

;; [[file:config.org::*TIDE][TIDE:1]]
(use-package tide
  :defer t)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'before-save-hook 'tide-format-before-save)
;; TIDE:1 ends here

;; [[file:config.org::*Typescript-Mode][Typescript-Mode:1]]
(use-package typescript-mode
  :mode "\\.ts"
  :diminish typescript-mode
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-hook 'typescript-mode-hook 'setup-tide-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
;; Typescript-Mode:1 ends here

;; [[file:config.org::*Web-Mode][Web-Mode:1]]
(use-package web-mode
  :ensure t
  :mode (("\\.tsx\\'" . web-mode))
  :init
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook (lambda () (pcase (file-name-extension buffer-file-name)
                      ("tsx" (setup-tide-mode))
                      (_ (my-web-mode-hook)))))
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-block-padding 2
   web-mode-comment-style 2
   web-mode-enable-css-colorization t
   web-mode-enable-auto-pairing t
   web-mode-enable-current-column-highlight t
   web-mode-auto-close-style 2
   web-mode-enable-auto-quoting nil
   web-mode-enable-auto-closing t
   web-mode-enable-current-element-highlight t
   web-mode-enable-auto-indentation nil))
(eval-after-load "web-mode"
  '(set-face-foreground 'web-mode-current-element-highlight-face "#ff9900"))
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
