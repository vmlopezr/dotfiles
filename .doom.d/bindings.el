
;; Set custom keybindings
(map!
 (:leader
  :desc "Focus on treemacs" "o m" #'treemacs-select-window
  :desc "dumb-jump-go" "g d" #'dumb-jump-go
  :desc "dumb-jump-back" "g f" #'dumb-jump-back
  :desc "dumb-jump-quick-look" "g q" #'dumb-jump-quick-look
  :desc "dumb-jump-go-other-window" "g w" #'dumb-jump-go-other-window)
  ;; Make esc exit any popup
 (:map
  (minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map)
    [escape] #'abort-recursive-edit
    "C-r" #'evil-paste-from-register))
