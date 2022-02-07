(defconst lispy-packages
  '(lispy
    lispyville))

(defun lispy/init-lispyville ()
  (use-package lispyville
    ;; :init
    ;; (add-hook 'lispy-mode-hook #'lispyville-mode)
    :hook ((clojure-mode cider-repl-mode emacs-lisp-mode lisp-mode scheme-mode) .
           lispyville-mode)
    :config
    (progn
      (diminish 'lispyville-mode (lispyville-mode-line-string "🍰" "🍰"))
      (lispy-define-key lispy-mode-map "v" #'lispyville-toggle-mark-type)
      (lispyville-set-key-theme '(additional
                                  additional-insert
                                  additional-motions
                                  additional-wrap
                                  atom-movement
                                  c-u
                                  c-w
                                  commentary
                                  escape
                                  mark-toggle
                                  operators
                                  prettify
                                  slurp/barf-lispy
                                  text-objects)))))

(defun lispy/init-lispy ()
  (use-package lispy
    :defer t
    :diminish lispy-mode ""
    :hook ((clojure-mode cider-repl-mode emacs-lisp-mode lisp-mode scheme-mode) .
           lispy-mode)
    :custom
    (lispy-compat '(edebug cider magit-blame-mode))
    (lispy-eval-display-style 'overlay)
    :config
    (defun lispy-cider-define-key (k f)
      (lispy-define-key lispy-mode-map-special k (lookup-key lispy-mode-map-special k)
        :override `(cond ((bound-and-true-p cider-mode)
                          (save-excursion
                            (when (looking-at lispy-left)
                              (lispy-different))
                            (,f))))))
    (progn
      (define-key lispy-mode-map-lispy (kbd "<M-return>") nil)
      (define-key lispy-mode-map-evilcp (kbd "<M-return>") nil)
      (define-key lispy-mode-map-lispy (kbd "M-RET") nil)
      ;; (lispy-cider-define-key "e" 'cider-eval-last-sexp)
      ;; (lispy-cider-define-key "E" 'cider-pprint-eval-last-sexp-to-comment)
      (evil-define-key 'insert lispy-mode-map
        (kbd "/")   'special-lispy-splice
        (kbd ")")   'lispy-right-nostring
        (kbd "C-d") 'lispy-delete
        (kbd "C-y") 'lispy-yank
        (kbd "C-e") 'lispy-move-end-of-line
        (kbd "M-.") 'lispy-goto-symbol))))

;;; packages.el ends here
