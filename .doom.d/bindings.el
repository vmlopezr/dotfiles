
;; Set custom keybindings
(map!
 (:leader
  :desc "Focus on treemacs" "o m" #'treemacs-select-window
  :desc "dumb-jump-go-back" "g b" #'dumb-jump-back
  :desc "dumb-jump-go" "g d" #'dumb-jump-go
  :desc "dumb-jump-quick-look" "g q" #'dumb-jump-quick-look
  :desc "dumb-jump-go-other-window" "g w" #'dumb-jump-go-other-window
  :desc "Toggle rainbow mode" "r b" #'rainbow-mode)

  ;; Make esc exit any popup
 (:map
  (minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map)
    [escape] #'abort-recursive-edit
    "C-r" #'evil-paste-from-register))
(map!
 "S-C-l" #'windsize-right
 "S-C-h" #'windsize-left
 "S-C-j" #'windsize-down
 "S-C-k" #'windsize-up
 "M-j" #'evil-window-rotate-downwards
 "M-k" #'evil-window-rotate-upwards)
