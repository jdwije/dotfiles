;;; Emacs --- jdw's emacs config file
;;;  
;;; Commentary: 
;;;  
;;;  This is my Emacs config. Hack away at it to your liking! I've copied bits
;;;  and pieces of it from other, more wiser folks and added my own
;;;  customization's to it. Here's a list of my sources.
;;;  
;;;  - Steve Yeggie: http://steve.yegge.googlepages.com/my-dot-emacs-file
;;;  - Aaron Bedra: http://aaronbedra.com/emacs.d/
;;;  

;;; Code:

;; User details
(setq user-full-name "Jason Wijegooneratne")
(setq user-mail-address "admin@jwije.com")

;; Package Management
(require 'package)

(package-initialize "no-activate")

;; Additonal repositories
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
;; (add-to-list 'load-path
   ;;           "~/.emacs.d/src/yasnippet")

(setq package-archive-enable-alist '(("melpa" deft magit)))

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(when (not (package-installed-p 'cl-lib))
  (package-install 'cl-lib))

(add-to-list 'load-path "~/.emacs.d/cl-lib/")
(require 'cl-lib)

;; (add-to-list 'load-path "~/.emacs.d/eslint-flycheck/")
;; (require 'eslint-flycheck)

(defvar default-packages
  '(ac-js2
    ac-helm
    auctex
    auto-complete
    coffee-mode
    column-enforce-mode
    company
    dash
    emr
    exec-path-from-shell
    expand-region
    feature-mode
    fish-mode
    flycheck
    flymake-easy
    flymake-jslint
    flymake-ruby
    go-mode
    helm
    helm-flx
    helm-flycheck
    helm-fuzzier
    helm-fuzzy-find
    helm-gitignore
    helm-grepint
    helm-ispell
    highlight-chars
    inf-ruby
    intero
    jade-mode
    js2-mode
    key-chord
    less-css-mode
    list-utils
    markdown-mode
    markdown-toc
    multiple-cursors
    paredit
    pkg-info
    popup
    projectile
    racer
    ruby-block
    ruby-electric
    rust-mode
    rw-hunspell
    s
    scss-mode
    slime
    smartparens
    sublimity
    tide
    use-package
    wakatime-mode
    web-mode
    yaml-mode
    yari
    yasnippet
    yasnippet-snippets)
  "A list of packages to ensure are installed at launch.")

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package default-packages)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;
;; PACKAGE SETUP ;;
;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(require 'auto-complete)
(require 'smartparens-config)
(require 'multiple-cursors)
(require 'auto-complete-config)
(require 'highlight-chars)
(require 'flycheck)
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map) ;; experimental
(require 'sublimity-attractive)
(require 'helm-config)

;; (sublimity-mode 1)

(setq sublimity-map-size 20)
(setq sublimity-map-fraction 0.3)
(setq sublimity-map-text-scale -7)
(sublimity-map-set-delay 2)
(add-hook 'sublimity-map-setup-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monospace"))
            (buffer-face-mode)))
(sublimity-attractive-hide-bars)

(winner-mode)
(projectile-mode)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(ac-config-default)
;; (ac-complete-yasnippet)
(put 'downcase-region 'disabled nil)
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; USER CONFIGURABLES ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq rcirc-server-alist
      '(("irc.freenode.net" :port 6697 :encryption tls
         :channels ("#rcirc" "#emacs" "#emacswiki"))))
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-date))
;; (yas-global-mode)

(setq ac-sources '(ac-source-semantic ac-source-yasnippet))
;; set autosaves to temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq org-agenda-include-diary t)
(setq diary-file "~/Organiser/Diary")
(setq org-agenda-files '("~/Organiser/agendas") )
(setq ruby-insert-encoding-magic-comment nil)
(setq flyspell-issue-welcome-flag nil)
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)
(setq tab-width 8
      indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(setq column-number-mode t)
(auto-compression-mode 1)
(setq ac-auto-start 2)
(setq ac-use-menu-map t)
(setq-default fill-column 80) ;; 80 character rule

;; smex setup
;;   (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;                     ; when Smex is auto-initialized on its first run.
;; (global-set-key (kbd "M-x") 'smex)
;;   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;   ;; This is your old M-x.
;;   (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; slime setup
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; JS setup
(setq js-indent-level 2)

;; fix up path for LATEX on OSX
(getenv "PATH")
(setenv "PATH"
        (concat
         "/Library/TeX/texbin/pdflatex" ":"
         (getenv "PATH")))

;; add /usr/local/bin to execution paths
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;; (setq exec-path (append exec-path '("/usr/local/bin")))

(setenv "PATH" (concat (getenv "PATH") ":/home/jdw/.nvm/versions/node/v4.3.2/bin"))
(setq exec-path (append exec-path '("/home/jdw/.nvm/versions/node/v4.3.2/bin")))


;; ispell setup
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;;;;;;;;;;;
;; LOOKS ;;
;;;;;;;;;;;

;; set a theme for GUI and terminal modes
(if window-system
    (load-theme 'evenhold t)
  (load-theme 'wombat t))

(if window-system
    (tool-bar-mode -1)
  nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;;;;;;;;;;;;;;;;;
;; USER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;

;; Go fullscreen on current frame
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
      (if (equal 'fullboth current-value)
        (if (boundp 'old-fullscreen) old-fullscreen nil)
        (progn (setq old-fullscreen current-value)
          'fullboth)))))

;; renames a buffer and its underlying file to the specified name
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; moves file underlying buffer and updates the buffer path accordingly
(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
         (if (string-match dir "\\(?:/\\|\\\\)$")
         (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

 (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
 (progn         (copy-file filename newname 1)  (delete-file filename)  (set-visited-file-name newname)         (set-buffer-modified-p nil)     t)))) 

;; function to bind ri ruby inline documentation to a key
(defun ri-bind-key ()
   (local-set-key [f1] 'yari))


(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

;; insert current datetime at cursor
(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       )

;; insert current time at cursor
(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       )

(defun copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))


;;  in buffer build system
(setq build-buffer "*ci-shell*")
(setq build-cmd "ant\n")

(defun build-in-buffer ()
  "Send a build command to a running shell"
  (interactive)
  (process-send-string build-buffer build-cmd))

(defun enable-in-buffer-builds (buf cmd)
  (interactive "bSpecify target buffer: \nsSpecify build command to send to buffer %s:")
  (setq build-buffer buf)
  (setq build-cmd cmd)
  (add-hook 'after-save-hook 'build-in-buffer)
  )

(defun disable-in-buffer-builds ()
  (interactive)
  (remove-hook 'after-save-hook 'build-in-buffer)
  )

;; zip/unzip files in dired mode
(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))

(eval-after-load "dired"
  '(define-key dired-mode-map "z" 'dired-zip-files))
(defun dired-zip-files (zip-file)
  "Create an archive containing the marked files."
  (interactive "sEnter name of zip file: ")

  ;; create the zip file
  (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
    (shell-command
     (concat "zip "
             zip-file
             " "
             (concat-string-list
              (mapcar
               #'(lambda (filename)
                  (file-name-nondirectory filename))
               (dired-get-marked-files))))))

  (revert-buffer)

  ;; remove the mark on all the files  "*" to " "
  ;; (dired-change-marks 42 ?\040)
  ;; mark zip file
  ;; (dired-mark-files-regexp (filename-to-regexp zip-file))
  )

(defun concat-string-list (list)
   "Return a string which is a concatenation of all elements of the list separated by spaces"
   (mapconcat #'(lambda (obj) (format "%s" obj)) list " "))

(defun xml-pretty-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
  (message "Ah, much better!"))


;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))


;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(defun setup-paths-osx ()
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;;; Fix junk characters in shell-mode
(add-hook 'shell-mode-hook 
          'ansi-color-for-comint-mode-on)

;; Flycheck setup
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; javascript

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(setq rust-format-on-save t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (red-alert)))
 '(custom-safe-themes
   (quote
    ("f34495514c7767496e94c3d4435b8d87b8923d1c52ab4e1978055cdb5c1bdec0" "17dd13452c80023a5a050faac15184369e493492fdbd6b151142ad24decd9240" "a5ebdbb839e09d37ed009840a0aa1ce60aaf6046940925414e825c6e84ccac11" "548dbeb21ab9abfba46f2911e7377c6d8eb3bf603e614f7f1c85e8d72893126a" "d7257a8bf161b46618199a67a2f41210464125230e63fc2d1792e5c71cd63003" "deaa09dad16f7f2dac6c82d69da9ab26e05c9f46942ab7fee02d51f3db29add8" "61df1a6f6cffdcce5bf5e81ab89015688602170079c42f6a8025b6c16f9661e8" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" default)))
 '(fci-rule-color "#3E4451")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(package-selected-packages
   (quote
    (atom-one-dark-theme flycheck-rust racer yari yaml-mode web-mode wakatime-mode use-package tidy tide smartparens slime scss-mode rw-hunspell ruby-electric ruby-block php-extras php-auto-yasnippets php+-mode multiple-cursors markdown-toc less-css-mode key-chord jsx-mode jade-mode intero inf-ruby highlight-chars go-mode flymake-ruby flymake-phpcs flymake-php flymake-json flymake-jslint flymake-jshint fish-mode feature-mode exec-path-from-shell emr column-enforce-mode coffee-mode auctex ac-js2 ac-c-headers)))
 '(wakatime-python-bin nil))

;; wakatime
(setq wakatime-api-key "2579cd0a-ac6b-4065-85f4-c1c2116d360a")
(setq wakatime-cli-path "/usr/local/bin/wakatime")

;; tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)


;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS ;;
;;;;;;;;;;;;;;;;;;

;; function shortcuts

;; replace backspace with backwards kill word
(global-set-key (kbd "DEL") 'backward-kill-word)
(global-set-key (kbd "M-DEL") 'backward-delete-char)

;; multiple cursors
(global-set-key (kbd "C-?") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

; set f5 + f6 to horizontal window size. set f7 + f8 to vertical window size.
(global-set-key (kbd "<f5>") 'shrink-window-horizontally)
(global-set-key (kbd "<f6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f7>") 'shrink-window)
(global-set-key (kbd "<f8>") 'enlarge-window)
(global-set-key (kbd "C-s-,") 'toggle-fullscreen)

;; utility
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(global-set-key "\C-c\C-t" 'insert-current-date-time)
(global-set-key (kbd "C-c qr") 'query-replace-regexp)
(global-set-key (kbd "C-c df") 'vc-diff)
(global-set-key (kbd "C-c cf") 'vc-next-action)
(global-set-key (kbd "C-c fg") 'rgrep)
(global-set-key (kbd "C-c ff") 'find-name-dired)
(global-set-key (kbd "C-c z") 'zap-to-char)

(global-set-key (kbd "M-s") 'yas/insert-snippet)

;;;;;;;;;;;;;;;;;;;
;; AUTO-COMPLETE ;;
;;;;;;;;;;;;;;;;;;;
(require 'ac-helm)  ;; Not necessary if using ELPA package

(define-key ac-mode-map (kbd "TAB") 'ac-complete-with-helm)
(define-key ac-completing-map (kbd "M-TAB") 'ac-complete)
(define-key ac-completing-map (kbd "RET") nil)

;;;;;;;;;;;;;;
;; WINDMOVE ;;
;;;;;;;;;;;;;;

(windmove-default-keybindings)

;;;;;;;;;;
;; HELM ;;
;;;;;;;;;;

(setq helm-recentf-fuzzy-match 1)
(setq helm-buffers-fuzzy-matching 1)
(setq helm-recentf-fuzzy-match 1)
(setq helm-buffers-fuzzy-matching 1)
(setq helm-locate-fuzzy-match 1)
(setq helm-M-x-fuzzy-match 1)
(setq helm-semantic-fuzzy-match 1)
(setq helm-imenu-fuzzy-match 1)
(setq helm-apropos-fuzzy-match 1)
(setq helm-lisp-fuzzy-completion 1)
(setq helm-mode-fuzzy-match 1)
(setq helm-completion-in-region-fuzzy-match 1)
(setq helm-candidate-number-limit 25)

(require 'helm-fuzzier)

(setq helm-flx-for-helm-find-files t ;; t by default
      helm-flx-for-helm-locate t) ;; nil by default

(helm-flx-mode +1)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x <up>") #'helm-ispell)

(helm-mode 1)
(helm-fuzzier-mode 1)


(require 'helm-grepint)
    (helm-grepint-set-default-config-latest)
    (global-set-key (kbd "C-c g") #'helm-grepint-grep)
(global-set-key (kbd "C-c G") #'helm-grepint-grep-root)

(global-set-key (kbd "C-c G") #'helm-grepint-grep-root)
(global-set-key (kbd "C-c G") #'helm-grepint-grep-root)


;;;;;;;;;;;;;;;
;; KEYCHORDS ;;
;;;;;;;;;;;;;;;

(key-chord-define-global "q/" 'enlarge-window)
(key-chord-define-global ".w" 'shrink-window)
(key-chord-define-global "z]" 'enlarge-window-horizontally)
(key-chord-define-global "x[" 'shrink-window-horizontally)
(key-chord-define-global "q]" 'balance-windows)
(key-chord-define-global "as" 'helm-occur)


(key-chord-mode +1) ;; always on



;;;;;;;;;;;;;
;; ALIASES ;;
;;;;;;;;;;;;;

(defalias 'yes-or-no-p 'y-or-n-p)

(setq alt-keysym 'meta)


;;;;;;;;;;;
;; HOOKS ;;
;;;;;;;;;;;

(add-hook 'after-init-hook 'setup-paths-osx)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'prog-mode-hook 'emr-initialize)
(add-hook 'after-init-hook #'smartparens-global-mode)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'ruby-mode-hook 'ri-bind-key)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-wakatime-mode)
(add-hook 'haskell-mode-hook 'intero-mode)

;;;;;;;;;;;;;;;;;;;
;; SPLASH SCREEN ;;
;;;;;;;;;;;;;;;;;;;

(setq splash-art "


 ▄▄▄██▀▀▀▓█████▄  █     █░ ░ ██████    ▓█████  ███▄ ▄███▓ ▄▄▄       ▄████▄    ██████ 
   ▒██   ▒██▀ ██▌▓█░ █ ░█░  ██    ▒    ▓█   ▀ ▓██▒▀█▀ ██▒▒████▄    ▒██▀ ▀█  ▒██    ▒ 
   ░██   ░██   █▌▒█░ █ ░█    ▓██▄      ▒███   ▓██    ▓██░▒██  ▀█▄  ▒▓█    ▄ ░ ▓██▄   
▓██▄██▓  ░▓█▄   ▌░█░ █ ░█    ▒   ██▒   ▒▓█  ▄ ▒██    ▒██ ░██▄▄▄▄██ ▒▓▓▄ ▄██▒  ▒   ██▒
 ▓███▒   ░▒████▓ ░░██▒██▓  ▒██████▒▒   ░▒████▒▒██▒   ░██▒ ▓█   ▓██▒▒ ▓███▀ ░▒██████▒▒
 ▒▓▒▒░    ▒▒▓  ▒ ░ ▓░▒ ▒   ▒ ▒▓▒ ▒ ░   ░░ ▒░ ░░ ▒░   ░  ░ ▒▒   ▓▒█░░ ░▒ ▒  ░▒ ▒▓▒ ▒ ░
 ▒ ░▒░    ░ ▒  ▒   ▒ ░ ░   ░ ░▒  ░ ░    ░ ░  ░░  ░      ░  ▒   ▒▒ ░  ░  ▒   ░ ░▒  ░ ░
 ░ ░ ░    ░ ░  ░   ░   ░   ░  ░  ░        ░   ░      ░     ░   ▒   ░        ░  ░  ░  
 ░   ░      ░        ░          ░        ░  ░       ░         ░  ░░ ░            ░  
          ░                                                       ░                 ")


(get-buffer-create "*splash*")
(switch-to-buffer "*splash*")
(insert-current-date-time)
(insert splash-art)
(beginning-of-buffer)
(read-only-mode)

(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
