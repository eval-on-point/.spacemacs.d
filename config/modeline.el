(setq-default all-the-icons-fileicon-scale-factor 0.76)
(setq-default all-the-icons-default-fileicon-adjust 0.08)

(setq-default all-the-icons-octicon-scale-factor 0.80)
(setq-default all-the-icons-alltheicon-scale-factor 0.72)
(setq-default all-the-icons-faicon-scale-factor 0.90)

(defun parsed-modeline-info ()
  (substring (nth 1 (split-string
                     (cider--modeline-info) ":"))
             0 -1))

(defun my-cider-lighter ()
  `((:propertize ,(if (string= "not connected" (cider--modeline-info))
                      "🍺"
                    (format "🍻[%s]" (parsed-modeline-info)))
                 face
                 ,(pcase (cider--modeline-info)
                    ("not connected" '((t :inherit warning)))
                    ((rx "cljs") '((t :inherit all-the-icons-cyan)))
                    ((rx "clj") '((t :inherit success)))
                    (_ '((t :inherit error)))))))

(setq cider-mode-line
      '(:eval (my-cider-lighter)))
(setq spaceline-org-clock-p t)

;; (defun spaceline-custom-theme (&rest additional-segments)
;;   (setq spaceline-all-the-icons-highlight-file-name t)
;;   (spaceline-all-the-icons-theme)
;;   (spaceline-all-the-icons--setup-git-ahead) ;; Enable # of commits ahead of upstream in git
;;   (spaceline-all-the-icons--setup-anzu)
;;
;;   (spaceline-define-segment my-mode-icon
;;     "An `all-the-icons' segment indicating the current buffer's mode with an icon"
;;     (let ((icon (all-the-icons-icon-for-mode major-mode)))
;;       (unless (symbolp icon)
;;         (propertize icon))))
;;
;;   (spaceline-compile
;;     ;; left side
;;     '(((persp-name
;;         workspace-number
;;         window-number)
;;        :fallback evil-state
;;        :face highlight-face
;;        :priority 100)
;;       (purpose :priority 94)
;;       ((my-mode-icon :priority 79)
;;        ((buffer-id
;;          remote-host
;;          buffer-size)
;;         :priority 98)
;;        all-the-icons-modified
;;        modified)
;;       auto-compile
;;       (process :when active)
;;       ((flycheck-error flycheck-warning flycheck-info)
;;        :when active
;;        :priority 89)
;;       (minor-modes :priority 9)
;;       ((all-the-icons-vc-icon
;;         all-the-icons-vc-status
;;         (all-the-icons-git-ahead
;;          (all-the-icons-git-status
;;           :tight-right t)))
;;        :face 'mode-line)
;;       ((org-pomodoro :when active)
;;        (org-clock :when active))
;;       (anzu :face 'mode-line))
;;     ;; right side
;;     '((python-pyvenv :fallback python-pyenv)
;;       (selection-info :priority 95)
;;       input-method
;;       (global :when active)
;;       (column :priority 99)))
;;   (spaceline-toggle-buffer-size-off)
;;   (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
;;   (spacemacs|diminish clj-refactor-mode)
;;   (spacemacs|diminish which-key-mode)
;;   (spacemacs|diminish yas-minor-mode)
;;   (spacemacs|diminish company-mode)
;;   (spacemacs|diminish auto-fill-function)
;;   (spacemacs|diminish smartparens-mode)
;;   (spacemacs|diminish evil-cleverparens-mode)
;;   (spacemacs|diminish spacemacs-whitespace-cleanup-mode)
;;   (spacemacs|diminish aggressive-indent-mode)
;;   (spacemacs|diminish column-enforce-mode)
;;   (spacemacs|diminish org-roam-mode)
;;   (spaceline-toggle-hud-off))
