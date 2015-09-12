
;;; EMACS --- jdw's emacs config file
;;; 
;;; This is my emacs config. Hack away at it to your liking! I've copied
;;; bits and pieces of it from other smart folks and added my own flavour
;;; to it. Here's a list of my sources. 
;;;
;;; INSPIRATION:
;;; - Steve Yeggie: http://steve.yegge.googlepages.com/my-dot-emacs-file
;;; - Aaron Bedra: http://aaronbedra.com/emacs.d/
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

(setq package-archive-enable-alist '(("melpa" deft magit)))

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/cl-lib/")
(require 'cl-lib)

(defvar default-packages
  '(ac-ispell
    auto-complete
    base16-theme 
    bongo 
    coffee-mode 
    dash
    discover 
    emr 
    epl
    figlet 
    flycheck 
    flymake-easy
    flymake-jshint 
    flymake-jslint
    flymake-php
    flymake-ruby
    flymake-yaml
    gandalf-theme
    hc-zenburn-theme
    inf-ruby
    key-chord
    less-css-mode
    list-utils
    makey
    markdown-mode
    markdown-mode+
    markdown-toc
    mmm-mode
    monochrome-theme
    multiple-cursors
    noctilux-theme
    organic-green-theme
    paredit
    pastels-on-dark-theme
    php+-mode
    php-auto-yasnippets
    php-mode
    pkg-info
    popup
    projectile
    redshank
    ruby-block
    ruby-dev
    ruby-electric
    ruby-refactor
    rw-hunspell
    s
    scss-mode
    smartparens
    web-mode
    yaml-mode
    yari
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun default-packages-installed-p ()
  (loop for p in default-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (default-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p default-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;;;;;;;;;;;;;;;;;;;
;; PACKAGE SETUP ;;
;;;;;;;;;;;;;;;;;;;

(require 'auto-complete)
(require 'smartparens-config)
(require 'yasnippet)
(require 'multiple-cursors)
(require 'php-auto-yasnippets)
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(ac-config-default)
(ac-complete-yasnippet)
(payas/ac-setup)
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
(setq yas-global-mode 1)
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
(setq tab-width 4
      indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(setq column-number-mode t)
(tool-bar-mode -1)
(auto-compression-mode 1)

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
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t)))) 

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
(setq build-buffer "*terminal*")
(setq build-cmd "vendor/bin/phpspec run\n")

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

;; php coding standards fixer
(defun php-cs-fix ()
  (interactive)
  (progn (shell-command (concat "php-cs-fixer fix " (buffer-file-name) " -v"))
	 (revert-buffer nil t)))

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

;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS ;;
;;;;;;;;;;;;;;;;;;

;; function shortcuts
(global-set-key (kbd "C-s-,") 'toggle-fullscreen)
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c qr") 'query-replace-regexp)
(global-set-key (kbd "C-c df") 'vc-diff)
(global-set-key (kbd "C-c cf") 'vc-next-action)
(global-set-key (kbd "C-c rg") 'rgrep)
(global-set-key (kbd "C-c ff") 'find-name-dired)

;; replace backspace with backwards kill word
(global-set-key (kbd "DEL") 'backward-kill-word)
(global-set-key (kbd "M-DEL") 'backward-delete-char)

; set f5 + f6 to horizontal window size. set f7 + f8 to vertical window size.
(global-set-key (kbd "<f5>") 'shrink-window-horizontally)
(global-set-key (kbd "<f6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f7>") 'shrink-window)
(global-set-key (kbd "<f8>") 'enlarge-window)
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(global-set-key "\C-c\C-t" 'insert-current-date-time)
;; (global-set-key "\C-c\C-t" 'insert-current-time)

(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;
;; HOOKS ;;
;;;;;;;;;;;

(add-hook 'prog-mode-hook 'emr-initialize)
(add-hook 'after-init-hook #'smartparens-global-mode)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'ruby-mode-hook 'ri-bind-key)
(add-hook 'after-init-hook 'global-flycheck-mode)


;;;;;;;;;;;;;;;;;;;
;; SPLASH SCREEN ;;
;;;;;;;;;;;;;;;;;;;

(setq splash-art "

welcome to...


 ▄▄▄██▀▀▀▓█████▄  █     █░  ██████    ▓█████  ███▄ ▄███▓ ▄▄▄       ▄████▄    ██████ 
   ▒██   ▒██▀ ██▌▓█░ █ ░█░▒██    ▒    ▓█   ▀ ▓██▒▀█▀ ██▒▒████▄    ▒██▀ ▀█  ▒██    ▒ 
   ░██   ░██   █▌▒█░ █ ░█ ░ ▓██▄      ▒███   ▓██    ▓██░▒██  ▀█▄  ▒▓█    ▄ ░ ▓██▄   
▓██▄██▓  ░▓█▄   ▌░█░ █ ░█   ▒   ██▒   ▒▓█  ▄ ▒██    ▒██ ░██▄▄▄▄██ ▒▓▓▄ ▄██▒  ▒   ██▒
 ▓███▒   ░▒████▓ ░░██▒██▓ ▒██████▒▒   ░▒████▒▒██▒   ░██▒ ▓█   ▓██▒▒ ▓███▀ ░▒██████▒▒
 ▒▓▒▒░    ▒▒▓  ▒ ░ ▓░▒ ▒  ▒ ▒▓▒ ▒ ░   ░░ ▒░ ░░ ▒░   ░  ░ ▒▒   ▓▒█░░ ░▒ ▒  ░▒ ▒▓▒ ▒ ░
 ▒ ░▒░    ░ ▒  ▒   ▒ ░ ░  ░ ░▒  ░ ░    ░ ░  ░░  ░      ░  ▒   ▒▒ ░  ░  ▒   ░ ░▒  ░ ░
 ░ ░ ░    ░ ░  ░   ░   ░  ░  ░  ░        ░   ░      ░     ░   ▒   ░        ░  ░  ░  
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aquamacs-customization-version-id 0)
 '(custom-enabled-themes (quote (evenhold)))
 '(custom-safe-themes
   (quote
    ("523d5a027e2f378ad80f9b368db450f4a5fa4a159ae11d5b66ccd78b3f5f807d" "557c715762e97e749c1c45d23f117056664dafd94465ec8c98d53e4929205a9c" "e12eca93c9766062e6ac435907a7df010f583d1c2d3c621279418a5c8f75566e" default)))
 '(flycheck-php-phpcs-executable "/Users/jdw/pear/bin/phpcs")
 '(one-buffer-one-frame-mode nil)
 '(package-archives
   (quote
    (("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("elpa" . "http://tromey.com/elpa/"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(tabbar-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
