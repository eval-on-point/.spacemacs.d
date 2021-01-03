;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private/")
   dotspacemacs-configuration-layers
   '(ansible
     asciidoc
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     (clojure :variables
              clojure-backend 'lsp
              clojure-enable-sayid nil
              clojure-enable-clj-refactor t
              clojure-enable-fancify-symbols t
              clojure-enable-linters '(clj-kondo)
              clojure-align-forms-automatically t
              clojure-indent-style 'align-arguments
              clojure-align-reader-conditionals t
              clojure--beginning-of-reader-conditional-regexp "\\[")
     (colors :variables colors-colorize-identifiers 'variables)
     command-log
     csv
     (docker :variables docker-dockerfile-backend 'lsp)
     (elfeed :variables
             rmh-elfeed-org-files '("~/org/feeds/feeds.org"
                                    "~/org/feeds/personal-feeds.org"))
     emacs-lisp
     emoji
     (exwm :variables
           exwm-workspace-display-echo-area-timeout 10
           exwm-floating-border-color "#586e75"

           exwm-workspace-number 1
           ;; (- (string-to-number
           ;;     (shell-command-to-string "xrandr --listactivemonitors|wc -l")) 1)

           exwm-enable-systray t
           exwm-autostart-xdg-applications nil
           exwm-locking-command "xss-lock -l -- ~/bin/transfer-sleep-lock-generic-delay.sh"
           exwm-install-logind-lock-handler t
           ;; exwm-custom-init (lambda() (exwm/autostart-process "Dunst OSD" "dunst"))
           )
     floobits
     git
     github
     haskell
     (helm :variables
           history-delete-duplicates t
           helm-enable-auto-resize t
           hybrid-style-enable-hjkl-bindings t
           helm-mini-default-sources '(helm-source-buffers-list
                                       helm-source-buffer-not-found)
           helm-buffer-max-length nil)
     helpful
     html
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     (javascript :variables javascript-backend 'lsp)
     (julia :variables julia-backend 'lsp)
     (json :variables json-backend 'lsp)
     kubernetes
     latex
     (lispy :variables
            lispy-completion-method 'helm
            lispy-clojure-middleware-tests nil)
     (lsp :variables
          lsp-enable-indentation nil
          lsp-enable-symbol-highlighting nil
          lsp-ui-doc-enable nil
          lsp-ui-doc-use-childframe t
          lsp-ui-sideline-show-code-actions nil
          lsp-ui-sideline-show-hover t
          lsp-file-watch-ignored-directories '("[/\\\\]\\.git\\'"
                                               "[/\\\\]\\.clj-kondo\\'"
                                               "[/\\\\]\\.lsp\\'"
                                               "[/\\\\]diagrams\\'"
                                               "[/\\\\]log\\'"
                                               "[/\\\\]node_modules\\'"
                                               "[/\\\\]resources\\'"
                                               "[/\\\\]target\\'")
          lsp-keymap-prefix "C-c C-l")
     major-modes
     (multiple-cursors :variables multiple-cursors-backend 'mc)
     (markdown :variables markdown-live-preview-engine 'vmd)
     nixos
     (org :variables
          org-startup-folded nil
          org-agenda-files '("~/org/" "~/org/journal" "~/org/personal/")
          org-agenda-file-regexp "\\`[^.].*\\.org$\\'"
          org-complete-tags-always-offer-all-agenda-tags t
          org-confirm-babel-evaluate nil
          org-enable-org-journal-support t
          org-journal-dir "~/org/journal/"
          org-journal-date-prefix "#+TITLE: "
          org-journal-time-prefix "* "
          org-journal-file-format "%Y-%m-%d.org"
          org-journal-time-format "[%F %R]"
          org-enable-github-support t
          org-babel-default-header-args:shell '((:results . ":output"))
          org-src-tab-acts-natively nil) ; see https://github.com/syl20bnr/spacemacs/issues/13465
     ;; org-roam
     (python :variables python-backend 'lsp
             python-lsp-server 'pyright
             python-test-runner 'pytest)
     (conda :variables
            conda-anaconda-home "/opt/miniconda3"
            conda-env-home-directory "~/.conda"
            conda-env-autoactivate-mode t)
     ranger
     (restclient :variables restclient-use-org t)
     (rust :variables
           rust-format-on-save t
           lsp-rust-server 'rust-analyzer)
     selectric
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'vterm
            vterm-always-compile-module t)
     (shell-scripts :variables shell-scripts-backend 'lsp)
     ;; slack
     (spacemacs-layouts :variables
                        spacemacs-layouts-directory "/home/m/.spacemacs.d/layouts/"
                        spacemacs-layouts-restrict-spc-tab t
                        persp-autokill-buffer-on-remove 'kill-weak)
     (spacemacs-purpose :variables
                        purpose-layout-dirs '("/home/m/.spacemacs.d/purpose-layouts/"))
     spell-checking
     (sql :variables
          sql-capitalize-keywords t
          sqlfmt-executable "pg_format"
          sqlfmt-options '("--wrap-limit" "80"))
     syntax-checking
     systemd
     terraform
     themes-megapack
     (treemacs :variables
               treemacs-use-scope-type 'Perspectives
               treemacs-use-follow-mode nil
               treemacs-use-filewatch-mode t
               treemacs-use-git-mode 'extended
               treemacs-use-icons-dired nil)
     (unicode-fonts :variables
                    unicode-fonts-enable-ligatures t
                    unicode-fonts-ligature-modes '(prog-mode vterm-mode)
	                  unicode-fonts-ligature-set
                    '("www" "**" "***" "/*" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                      ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                      "-<" "-<<" "-~" "#{" "#[" "#(" "#?" "#_"
                      "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                      "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                      "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                      "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                      "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                      "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                      "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
     version-control
     xclipboard
     (yaml :variables yaml-enable-lsp t)
     ;; local-dev
     )

   dotspacemacs-additional-packages
   '((org-fc
      :location (recipe :fetcher github
                        :repo "l3kn/org-fc"
                        :files (:defaults "awk" "demo.org")))
     (evil-adjust :location (recipe :fetcher github :repo "troyp/evil-adjust"))
     helm-rg
     delight
     kaocha-runner
     inf-clojure
     envrc
     edbi
     org-roam
     mixed-pitch
     clojure-snippets
     eterm-256color
     all-the-icons-dired)

   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(evil-escape
                                    evil-unimpaired
				                            all-the-icons)
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(bookmarks
                                (projects . 7)
                                (recents . 5))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         doom-fairy-floss
                         doom-solarized-light
                         doom-laserwave
                         doom-city-lights
                         doom-manegarm)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-mode-line-theme 'custom
   ;; dotspacemacs-mode-line-theme 'all-the-icons
   dotspacemacs-default-font '("FiraCode Nerd Font"
                               :size 8.0
                               :weight normal
                               :width normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-SPC"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "s-,"
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for sekparate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing))

(defun dotspacemacs/user-init ()

  ;; (defun spaceline-custom-theme (&rest additional-segments)
  ;;   "Custom spaceline theme."
  ;;   (spaceline-compile
  ;;     `(major-mode (minor-modes :whem active) buffer-id)
  ;;     `((line-column :separator " | " :priority 3)
  ;;       ,@additional-segments))
  ;;   (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

  (setq vc-follow-symlinks t)

  (load "~/.spacemacs.d/config/modeline.el")

  (defun spaceline-custom-theme (&rest additional-segments)
    (setq spaceline-all-the-icons-highlight-file-name t)
    (spaceline-all-the-icons-theme)
    (spaceline-all-the-icons--setup-git-ahead) ;; Enable # of commits ahead of upstream in git
    (spaceline-all-the-icons--setup-anzu)

    (spaceline-define-segment my-mode-icon
      "An `all-the-icons' segment indicating the current buffer's mode with an icon"
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (unless (symbolp icon)
          (propertize icon
                      'help-echo (format "Major-mode: `%s'" major-mode)))))
    (spaceline-compile
      ;; left side
      '(((persp-name
          workspace-number
          window-number)
         :fallback evil-state
         :face highlight-face
         :priority 100)
        (purpose :priority 94)
        (all-the-icons-modified
         buffer-id
         remote-host
         buffer-size)
        auto-compile
        (process :when active)
        (my-mode-icon
         minor-modes)
        (all-the-icons-vc-icon
         all-the-icons-vc-status
         ((all-the-icons-git-ahead
           all-the-icons-git-status) :separator " "))
        ((flycheck-error flycheck-warning flycheck-info)
         :when active
         :priority 89)
        ((org-pomodoro :when active)
         (org-clock :when active))
        (anzu :face 'mode-line))
      ;; right side
      `((python-pyvenv :fallback python-pyenv)
        (selection-info :priority 95)
        input-method
        (global :when active)
        (column :priority 99)))
    (spaceline-toggle-buffer-size-off)
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
    (spaceline-toggle-hud-off)
    )

  (setq exwm-custom-init
        (lambda ()
          (interactive)
          (persp-load-state-from-file "startup")
          (exwm/app-launcher "thunderbird")
          (exwm/app-launcher "discord")
          (exwm/app-launcher "slack")
          (sleep-for 4)
          (persp-switch "comm")
          (persp-do-buffer-list-by-regexp
           :regexp "X:.*"
           :func 'persp-add-buffer
           :rest-args (list (persp-add-new "comm") nil)
           :blist (persp-buffer-list-restricted (selected-frame) 1)
           :noask t)
          ;; (persp-add-buffers-by-regexp "X:.*" (persp-add-new "comm"))
          (eyebrowse-switch-to-window-config 1)
          (switch-to-first-matching-buffer "X:Slack.*")
          (eyebrowse-switch-to-window-config 2)
          (switch-to-first-matching-buffer "X:Thunderbird.*")
          (eyebrowse-switch-to-window-config 3)
          (switch-to-first-matching-buffer "X:discord.*")
          (eyebrowse-switch-to-window-config 1)
          (persp-switch "emacs")))
  (setq confirm-kill-processes nil)
  (setq kill-buffer-query-functions nil)
  (setq x-underline-at-descent-line t))

(defun dotspacemacs/user-config ()
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  (setq projectile-git-submodule-command nil)

  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (with-eval-after-load 'vterm
    (add-to-list 'vterm-eval-cmds
                 '("find-files"
                   (lambda (&rest ARGS)
                     (mapcar 'find-file ARGS)))))

  (load "~/.spacemacs.d/config/window-conf.el")

  (setq wdired-allow-to-change-permissions t)

  (require 'evil-adjust)
  (evil-adjust)

  (setq evil-insert-state-cursor '("#2aa198" (bar . 2)))

  (spacemacs/toggle-desktop-environment-on)

  (spacemacs/toggle-vi-tilde-fringe-off)

  (setq-default which-key-idle-delay 2)
  (setq-default which-key-show-early-on-C-h t)

  ;; (spacemacs/toggle-evil-visual-mark-mode-on)
  (setq-default evil-ex-search-vim-style-regexp t)
  (avy-setup-default)
  (defun switch-to-buffer--hack (orig-fun &rest args)
    (if-let ((win (get-buffer-window (car args))))
        (select-window win)
      (apply orig-fun args)))

  (define-key evil-normal-state-map (kbd "s") 'avy-goto-word-or-subword-1)
  ;; (add-to-list 'avy-subword-extra-word-chars "")

  (define-key evil-normal-state-map (kbd "C-h") 'avy-pop-mark)

  (advice-add 'switch-to-buffer :around #'switch-to-buffer--hack)

  (add-hook 'helm-mode-hook
            (lambda ()
              (add-to-list 'helm-completing-read-handlers-alist
                           '(org-set-tags-command))))

  (defun spacemacs/home () nil)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (require 'org-tempo)
  (setq org-structure-template-alist
        (append org-structure-template-alist '(("el" . "src emacs-lisp")
                                               ("sh" . "src shell")
                                               ("clj" . "src clojure")
                                               ("http" . "src http"))))
  (spacemacs/set-leader-keys "pf" (lambda ()
                                    (interactive)
                                    (if (projectile-project-p)
                                        (projectile-find-file)
                                      (let ((default-directory "~"))
                                        (helm-find nil)))))
  ;; (setq org-journal-find-file 'find-file)
  ;; (add-hook 'persp-created-functions
  ;;           (lambda (persp hash)
  ;;             (spacemacs-buffer/goto-buffer)
  ;;             (org-journal-new-entry t)
  ;;             (persp-add-buffer
  ;;              (get-buffer (format-time-string "%Y-%m-%d.org"))
  ;;              persp)))

  ;; (add-hook 'org-journal-after-header-create-hook
  ;;           (lambda ()
  ;;             (insert "\n\n* "
  ;;                     "Previous journal entry: "
  ;;                     "[[" (car (reverse (org-journal-list-files))) "]"
  ;;                     "[" (mapconcat 'int-to-string
  ;;                                    (car (reverse (org-journal-list-dates)))
  ;;                                    "-")
  ;;                     "]]")
  ;;             (org-agenda-file-to-front)
  ;;             (save-buffer)))

  ;; (add-hook 'org-journal-after-entry-create-hook
  ;;           (lambda ()
  ;;             (save-excursion
  ;;               (beginning-of-line)
  ;;               (insert "\n"))
  ;;             (save-buffer)))
  (setq browse-url-mosaic-program
        nil)
  ;;
  ;;  (with-eval-after-load 'org
  ;;    (load-file "~/.spacemacs.d/private/org-habit-plus/org-habit-plus.el")
  ;;    (add-to-list 'org-modules 'org-habit-plus t))

  (require 'org-fc-hydra)
  (setq org-fc-directories '("~/org/"))

  (server-start)

  ;; (fancy-battery-mode)
  ;; (setq symon-delay 8
  ;;       symon-sparkline-width 0
  ;;       symon-monitors
  ;;       '(symon-linux-memory-monitor
  ;;         symon-linux-cpu-monitor
  ;;         symon-linux-network-rx-monitor
  ;;         symon-linux-network-tx-monitor
  ;;         symon-linux-battery-monitor
  ;;         symon-current-time-monitor))
  ;; (symon-mode)

  (add-hook 'buffer-list-update-hook
            (lambda ()
              (when (string-prefix-p exwm-buffer-name-prefix
                                     (buffer-name (first (buffer-list))))
                (setq exwm-input-line-mode-passthrough nil))))

  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (message "it ran")
              ;; (start-process-shell-command
              ;;  "autorandr" "--change")
              (let ((surplus (- (exwm-workspace--count)
                                (string-to-number
                                 (shell-command-to-string
                                  "xrandr|grep \" connected\"|wc -l")))))
                (cond ((< 0 surplus) (dotimes (_ surplus)
                                       (exwm-workspace-delete)))
                      ((< surplus 0) (dotimes (_ (* -1 surplus))
                                       (exwm-workspace-add)))
                      (t nil)))))

  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (add-hook 'evil-insert-state-entry-hook
                        'evil-emacs-state
                        nil t)))

  (setq kill-buffer-query-functions
        (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  (defun add-hooks (hooks functions)
    (cond ((not functions) nil)
          ((consp functions)
           (dolist (fun functions)
             (add-hooks hooks fun)))
          (t
           (dolist (hook hooks)
             (add-hook hook functions)))))

  (add-hooks '(org-mode-hook
               markdown-mode-hook ;; slack-mode-hook
               treemacs-mode-hook)
             #'mixed-pitch-mode)

  (setq-default fill-column 120)
  (add-hooks '(prog-mode-hook text-mode-hook) #'auto-fill-mode)
  ;; (add-hook 'prog-mode-hook #'fci-mode)
  (add-hook 'prog-mode-hook #'column-enforce-mode)
  (setq-default column-enforce-column 120)
  (setq projectile-indexing-method 'hybrid)

  ;; (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)

  (spacemacs/declare-prefix "o" "custom")

  (setq-default treemacs-show-hidden-files nil
                treemacs-width 25)

  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)

  (add-hook 'prog-mode-hook #'rainbow-mode)


  (display-time-mode 0)
  (setq-default display-time-default-load-average nil)

  (setq-default js-indent-level 2)

  ;; (use-package lsp-mode
  ;;   :ensure t
  ;;   :hook ((clojure-mode . lsp)
  ;;          (clojurec-mode . lsp)
  ;;          (clojurescript-mode . lsp)))

  (setq cider-repl-display-help-banner nil
        cider-font-lock-dynamically '(macro core function var)
        nrepl-sync-request-timeout 120
        cider-stacktrace-default-filters '(project)
        cider-jdk-src-paths nil)

  (eval-after-load 'cider
    '(progn
       (cider-add-to-alist 'cider-jack-in-dependencies
                           "org.tcrawley/dynapath" "0.2.5")
       (cider-add-to-alist 'cider-jack-in-dependencies
                           "com.cemerick/pomegranate" "0.4.0")))

  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-allow-jack-in-without-project t)
  ;; (setq cider-clojure-cli-global-options "-A:scratch")

  (add-hook 'cider-inspector-mode-hook #'spacemacs/toggle-truncate-lines-on)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

  (setq cljr-warn-on-eval nil
        cljr-hotload-dependencies t)

  (spacemacs|use-package-add-hook clj-refactor
    :post-config
    (spacemacs|forall-clojure-modes m
      (dolist (r cljr--all-helpers)
        (let* ((binding (car r))
               (func (cadr r)))
          (unless (and (eq clojure-backend 'lsp)
                       (member binding '("cn" "el" "ml" "il" "is" "ef" "am")))
            (when (string-prefix-p "r" binding) (store-substring binding 0 ?R))
            (when (not (string-prefix-p "hydra" (symbol-name func)))
              (spacemacs/set-leader-keys-for-major-mode m
                (concat "r" binding) func)))))
      (when (eq clojure-backend 'lsp)
        (spacemacs/set-leader-keys-for-major-mode m
          "rcn" 'lsp-clojure-clean-ns
          "rel" 'lsp-clojure-expand-let
          "rml" 'lsp-clojure-move-to-let
          "ril" 'lsp-clojure-introduce-let
          "ris" 'lsp-clojure-inline-symbol
          "ref" 'lsp-clojure-extract-function
          "ram" 'lsp-clojure-add-missing-libspec))))

  (with-eval-after-load 'clojure-mode
    (clojure-snippets-initialize)
    (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "sjb" 'bb-jack-in)
    (add-to-list 'clojure-align-cond-forms "array-map")
    (define-clojure-indent
      (prop/for-all 1)
      (async 1)
      (rf/reg-event-fx 'defun)
      (rf/reg-event-db 'defun)
      (reg-event-db 'defun)
      (rf/reg-sub 'defun)
      (fn-traced 1)
      (defroutes 'defun)
      (cond-> 1)
      (cond->> 1)
      (as-> 2)
      (as->> 2)
      (tp/it-> 1)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (OPTIONS 2)
      (PATCH 2)
      (rfn 2)
      (let-routes 1)
      (context 2)))



  ;; (add-hook 'cider-mode-hook
  ;;           (lambda ()
  ;;             (defun cider-eval-sexp-at-point-to-buffer ()
  ;;               (interactive)
  ;;               (cider-pprint-eval-last-sexp t))
  ;;             (lispy-define-key lispy-mode-map "e" 'cider-eval-sexp-at-point)
  ;;             (lispy-define-key lispy-mode-map "E" 'cider-eval-sexp-at-point-to-buffer)))

  (defun bb-jack-in ()
    (interactive)
    (start-process "bb" "bb-nrepl" "bb" "--nrepl-server")
    (cider-connect '(:port 1667
                           :host "localhost")))
  (defun bb-connect ()
    (interactive)
    (cider-connect '(:port 1667
                           :host "localhost")))

  (setq-default cljr-auto-sort-ns nil
                cljr-auto-clean-ns nil
                cljr-favor-prefix-notation t)

  (with-eval-after-load 'clj-refactor
    (lispy-define-key lispy-mode-map "/" 'lispy-splice
      :inserter 'cljr-slash)

    (setq cljr-magic-requires-namespaces
          '(("csv" . "clojure.data.csv")
            ("gdom" . "goog.dom")
            ("ig" . "integrant.core")
            ("io" . "clojure.java.io")
            ("json" . "cheshire.core")
            ("r" . "reagent.core")
            ("rf" . "re-frame.core")
            ("set" . "clojure.set")
            ("string" . "clojure.string")
            ("t" . "clojure.test")
            ("walk" . "clojure.walk")
            ("zip" . "clojure.zip"))))

  (defun cider--guess-evaluation-context ()
    "returns list of let-binding strings from the inside out, without closing parens
     \"(let [...]\""
    (save-excursion
      (let ((res ()))
        (condition-case er
            (while t
              (backward-up-list)
              (when (looking-at (rx "(" (or "when-let" "if-let" "let") (opt "*")
                                    symbol-end (* space)
                                    (group "["))) ;; binding vector
                (let ((beg (match-beginning 0))
                      (end (save-excursion
                             (goto-char (match-beginning 1))
                             (forward-sexp 1)
                             (point))))
                  (push (buffer-substring-no-properties beg end) res))))
          (scan-error res)))))

  (defun cider-eval-dwim ()
    (interactive)
    (let ((ctx (cider--guess-evaluation-context))
          (bounds (cider-sexp-at-point 'bounds)))
      (cider-interactive-eval (concat (apply #'concat ctx)
                                      (buffer-substring-no-properties (car bounds) (cadr bounds))
                                      (make-string (length ctx) ?\)))
                              nil bounds
                              (cider--nrepl-pr-request-map))))

  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "ta" 'cider-test-run-project-tests
    "tn" 'cider-test-run-ns-tests
    "tt" 'cider-test-run-test
    "oe;"
    (lambda ()
      (interactive)
      (evil-insert-state)
      (forward-char)
      (newline)
      (cider-pprint-eval-last-sexp-to-comment)
      (evil-normal-state))
    "oec" 'cider-eval-dwim
    "oei" 'eval-sexp-fu-cider-eval-sexp-inner-list
    "oi" 'cider-inspect-last-result
    "os" 'cider-selector)



  (with-eval-after-load "cider-inspector"
    (define-key cider-inspector-mode-map
      (kbd "f") 'ace-link-cider-inspector))

  ;; rust setup
  ;; see https://github.com/kwrooijen/cargo.el/issues/29 for more info
  (with-eval-after-load 'rust-mode
    (define-key rust-mode-map (kbd "C-q") 'my-cargo-run))

  (defun my-cargo-run ()
    "Build and run Rust code."
    (interactive)
    (cargo-process-run)
    (let ((orig-win (selected-window))
          (run-win (display-buffer (get-buffer "*Cargo Run*") nil 'visible)))
      (select-window run-win)
      (comint-mode)
      (read-only-mode 0)
      (end-of-buffer)))

  (use-package yasnippet
    :diminish 'yas-minor-mode
    :config
    (progn
      (defun yas-no-expand-in-comment/string ()
        (setq yas-buffer-local-condition
              '(if (nth 8 (syntax-ppss)) ;; non-nil if in a string or comment
                   '(require-snippet-condition . force-in-comment)
                 t)))
      (add-hook 'prog-mode-hook 'yas-no-expand-in-comment/string)
      (evil-define-minor-mode-key 'insert
        'yas-minor-mode
        " "
        yas-maybe-expand)
      (setq yas-key-syntaxes '(yas-try-key-from-whitespace))))

  ;; haskell setup
  (with-eval-after-load "haskell-mode"
    (defun haskell-evil-open-above ()
      (interactive)
      (evil-digit-argument-or-evil-beginning-of-line)
      (haskell-indentation-newline-and-indent)
      (evil-previous-line)
      (haskell-indentation-indent-line)
      (evil-append-line nil))

    (defun haskell-evil-open-below ()
      (interactive)
      (evil-append-line nil)
      (haskell-indentation-newline-and-indent))

    (evil-define-key 'normal haskell-mode-map "o" 'haskell-evil-open-below
      "O" 'haskell-evil-open-above))

  (defun match-buffer-name (regex b)
    (string-match-p regex (buffer-name b)))

  (defun switch-to-first-matching-buffer (regex)
    (switch-to-buffer
     (car (remove-if-not (apply-partially #'match-buffer-name regex)
                         (buffer-list)))))

  (require 'exwm-randr)
  (exwm-randr-enable)

  (envrc-global-mode)
  (defun envrc--lighter ()
    "Return a colourised version of `envrc--status' for use in the mode line."
    `((:propertize "🌴"
                   face
                   ,(pcase envrc--status
                      (`on 'envrc-mode-line-on-face)
                      (`error 'envrc-mode-line-error-face)
                      (`none 'envrc-mode-line-none-face)))))

  (use-package lsp-mode
    :delight
    '(:eval
      (if lsp--buffer-workspaces
          '(:propertize "👓" face 'envrc-mode-line-on-face)
        '(:propertize "👓" face 'envrc-mode-line-none-face))))

  ;; (dolist (m
  ;;          (list clj-refactor-mode
  ;;                which-key-mode
  ;;                yas-minor-mode
  ;;                company-mode
  ;;                auto-fill-function
  ;;                smartparens-mode
  ;;                spacemacs-whitespace-cleanup-mode
  ;;                aggressive-indent-mode
  ;;                column-enforce-mode
  ;;                all-the-icons-dired-mode
  ;;                mixed-pitch-mode
  ;;                flyspell-mode
  ;;                flycheck-mode
  ;;                subword-mode
  ;;                company-mode
  ;;                smartparens-mode
  ;;                column-enforce-mode)
  ;;          nil)
  ;;   (eval-after-load 'm
  ;;     (spacemacs|diminish m)))

  (eval-after-load 'smartparens-mode
    (spacemacs|diminish smartparens-mode))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(highlight-parentheses-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
   '(package-selected-packages
     '(helm-rg delight zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode ws-butler writeroom-mode wolfram-mode winum white-sand-theme which-key web-mode web-beautify vterm volatile-highlights vmd-mode vi-tilde-fringe vala-snippets vala-mode uuidgen use-package unicode-fonts undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toxi-theme toml-mode toc-org thrift terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit systemd symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection stan-mode sqlup-mode sql-indent sphinx-doc spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme selectric-mode scss-mode scad-mode sass-mode ron-mode reverse-theme restclient-helm restart-emacs rebecca-theme ranger rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme racer qml-mode pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme prettier-js planet-theme pkgbuild-mode pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el password-generator paradox ox-gfm overseer orgit organic-green-theme org-superstar org-rich-yank org-projectile org-present org-pomodoro org-mime org-journal org-fc org-download org-cliplink org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http nodejs-repl noctilux-theme nix-mode naquadah-theme nameless mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme mmm-mode mixed-pitch minimal-theme matlab-mode material-theme markdown-toc majapahit-theme magit-svn magit-section magit-gitflow madhat2r-theme macrostep lush-theme lsp-ui lsp-python-ms lsp-pyright lsp-origami lsp-latex lsp-julia lsp-haskell lorem-ipsum logcat livid-mode live-py-mode lispyville link-hint light-soap-theme ligature kubernetes-tramp kubernetes-evil kaolin-themes kaocha-runner julia-repl json-navigator js2-refactor js-doc jinja2-mode jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme inf-clojure indent-guide importmagic impatient-mode ibuffer-projectile hybrid-mode hungry-delete hoon-mode hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helpful helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-nixos-options helm-mode-manager helm-make helm-lsp helm-ls-git helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-cider helm-c-yasnippet helm-ag hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme grip-mode grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitignore-templates github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ gist gh-md gandalf-theme fuzzy framemove forge font-lock+ flyspell-correct-helm flycheck-rust flycheck-pos-tip flycheck-package flycheck-haskell flycheck-elsa flycheck-clj-kondo flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode farmhouse-theme fancy-battery eziam-theme eyebrowse exwm expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-easymotion evil-cleverparens evil-args evil-anzu evil-adjust eterm-256color espresso-theme eshell-z eshell-prompt-extras esh-help envrc emr emojify emoji-cheat-sheet-plus emmet-mode elisp-slime-nav elfeed-org elfeed-goodies editorconfig edbi ebuild-mode dumb-jump dracula-theme dotenv-mode doom-themes dockerfile-mode docker django-theme dired-quick-sort diminish devdocs desktop-environment define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dap-mode dante dakrone-theme cython-mode cyberpunk-theme csv-mode conda company-web company-terraform company-statistics company-shell company-restclient company-reftex company-quickhelp company-nixos-options company-emoji company-cabal company-auctex company-ansible company-anaconda command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode cmm-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chocolate-theme cherry-blossom-theme centered-cursor-mode cargo busybee-theme bubbleberry-theme browse-at-remote blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk attrap arduino-mode apropospriate-theme anti-zenburn-theme ansible-doc ansible ample-zen-theme ample-theme all-the-icons-dired alect-themes aggressive-indent afternoon-theme adoc-mode ace-link ace-jump-helm-line ac-ispell))
   '(safe-local-variable-values
     '((lsp-file-watch-ignored-directories "[/\\\\]\\.git\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]diagrams\\'" "[/\\\\]log\\'" "[/\\\\]node_modules\\'" "[/\\\\]resources\\'" "[/\\\\]target\\'")
       (lsp-file-watch-ignored "\\.git$" "/resources$" "/\\.lsp$" "/diagrams" "/\\.clj-kondo$" "/node_modules$" "/target$" "/log$")
       (javascript-backend . tide)
       (javascript-backend . tern)
       (javascript-backend . lsp))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((((class color) (min-colors 89)) (:foreground "#839496" :background "#002b36")))))
  )
