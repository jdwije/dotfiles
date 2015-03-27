(add-to-list 'load-path "~/.emacs.d/cl-lib/")
(require 'cl-lib)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy t)
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#151718" "#CE4045" "#9FCA56" "#DCCD69" "#55B5DB" "#A074C4" "#55B5DB" "#D4D7D6"])
 '(ansi-term-color-vector [unspecified "#151718" "#CE4045" "#9FCA56" "#DCCD69" "#55B5DB" "#A074C4" "#55B5DB" "#D4D7D6"])
 '(background-color "#202020")
 '(background-mode dark)
 '(bongo-enabled-backends (quote (afplay)))
 '(bongo-insert-album-covers t)
 '(cursor-color "#cccccc")
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("a0fdc9976885513b03b000b57ddde04621d94c3a08f3042d1f6e2dbc336d25c7" "f15a7ce08b9e13553c1f230678e9ceb5b372f8da26c9fb815eb20df3492253b7" "e3c90203acbde2cf8016c6ba3f9c5300c97ddc63fcb78d84ca0a144d402eedc6" "2a86b339554590eb681ecf866b64ce4814d58e6d093966b1bf5a184acf78874d" "c537bf460334a1eca099e05a662699415f3971b438972bed499c5efeb821086b" "8fd393097ac6eabfcb172f656d781866beec05f27920a0691e8772aa2cdc7132" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "9eb5269753c507a2b48d74228b32dcfbb3d1dbfd30c66c0efed8218d28b8f0dc" "784d5ee4d33befba6a21702ead67f98346770be7cc17ab64952ae3866a403743" "61b188036ad811b11387fc1ef944441826c19ab6dcee5c67c7664a0bbd67a5b5" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "a0d036af9adade4e320600d773b31af5990051db2e8b0a736b01eb40b3f83881" "6c9ddb5e2ac58afb32358def7c68b6211f30dec8a92e44d2b9552141f76891b3" "0744f61189c62ed6d1f8fa69f6883d5772fe8577310b09e623c62c040f208cd4" "c739f435660ca9d9e77312cbb878d5d7fd31e386a7758c982fa54a49ffd47f6e" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" default)))
 '(fci-rule-color "#383838")
 '(foreground-color "#cccccc")
 '(fringe-mode 6 nil (fringe))
 '(highlight-symbol-colors (quote ("#EFFF00" "#73CD4F" "#83DDFF" "MediumPurple1" "#66CDAA" "DarkOrange" "HotPink1" "#809FFF" "#ADFF2F")))
 '(hl-paren-background-colors (quote ("#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00")))
 '(hl-paren-colors (quote ("#326B6B")))
 '(image-dired-append-when-browsing t)
 '(linum-format " %7d ")
 '(vc-annotate-background "#2b2b2b")
 '(vc-annotate-color-map (quote ((20 . "#bc8383") (40 . "#cc9393") (60 . "#dfaf8f") (80 . "#d0bf8f") (100 . "#e0cf9f") (120 . "#f0dfaf") (140 . "#5f7f5f") (160 . "#7f9f7f") (180 . "#8fb28f") (200 . "#9fc59f") (220 . "#afd8af") (240 . "#bfebbf") (260 . "#93e0e3") (280 . "#6ca0a3") (300 . "#7cb8bb") (320 . "#8cd0d3") (340 . "#94bff3") (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
;; make buffer [try] to stick to current window
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode)) 

;; Go fullscreen on current frame
;; CTR + CMD + ,  = Full Screen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
      (if (equal 'fullboth current-value)
        (if (boundp 'old-fullscreen) old-fullscreen nil)
        (progn (setq old-fullscreen current-value)
          'fullboth)))))

(global-set-key (kbd "C-s-,") 'toggle-fullscreen)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; $PATH fix for internal shells
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH)) 

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
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

(setq rcirc-server-alist
      '(("irc.freenode.net" :port 6697 :encryption tls
	 :channels ("#rcirc" "#emacs" "#emacswiki"))))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-date))


(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'auto-complete)
(require 'yasnippet)
(require 'multiple-cursors)
(require 'php-auto-yasnippets)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)

(ac-complete-yasnippet)

(setq yas-global-mode 1)

(setq ac-sources '(ac-source-semantic ac-source-yasnippet))

(setq php-auto-yasnippet-php-program "/Users/JWIJESWWW/.emacs.d/elpa/php-auto-yasnippets-20140324.1133/Create-PHP-YASnippet.php")

(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(payas/ac-setup)

;; set autosaves to temp dir

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Org Setup
(setq org-agenda-include-diary t)
(setq diary-file "~/Organiser/Diary")
(setq org-agenda-files '("~/Organiser/agendas") )

; (global-set-key "\C-cl" 'org-store-link)
 ;    (global-set-key "\C-cc" 'org-capture)
  ;   (global-set-key "\C-ca" 'org-agenda)
   ;  (global-set-key "\C-cb" 'org-iswitchb)



(put 'downcase-region 'disabled nil)


;; firefox linkage
(defun shell-command-on-buffer ()
  "Asks for a command and executes it in inferior shell with current buffer
as input."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "/Applications/Firefox.app/Contents/MacOS/firefox -new-tab")))

(global-set-key (kbd "C-'") 'shell-command-on-buffer) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD MAPPING CHANGES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; replace backspace with backwards kill word
(global-set-key (kbd "DEL") 'backward-kill-word)
(global-set-key (kbd "M-DEL") 'backward-delete-char)

; set f5 + f6 to horizontal window size. set f7 + f8 to vertical window size.
(global-set-key (kbd "<f5>") 'shrink-window-horizontally)
(global-set-key (kbd "<f6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f7>") 'shrink-window)
(global-set-key (kbd "<f8>") 'enlarge-window)

; http://kb.mozillazine.org/Command_line_arguments
; https://developer.mozilla.org/en-US/docs/Mozilla/Command_Line_Options
; http://www.emacswiki.org/emacs/ExecuteExternalCommand

(setq ruby-insert-encoding-magic-comment nil)

;; iSpell stuff
;; (setq ispell-program-name "hunspell")
;; (require 'rw-hunspell)


(setq default-ispell-program-name "aspell")

;; EMACS Grunt Build Command
(global-set-key (kbd "<f12>") 'grunt)
(setq grunt-cmd "grunt --no-color --config ~/grunt.js")
 
(defun grunt ()
  "Run grunt"
  (interactive)
  (let* ((grunt-buffer (get-buffer-create "*grunt*"))
        (result (call-process-shell-command grunt-cmd nil grunt-buffer t))
        (output (with-current-buffer grunt-buffer (buffer-string))))
    (cond ((zerop result)
           (message "Grunt completed without errors"))
          (t
           (message nil)
           (split-window-vertically)
           (set-window-buffer (next-window) grunt-buffer)))))






;;(when (require 'some-library nil 'noerror)
 ;;e do-things)

;; inf-ruby: run irb nicely in buffers
;; (when (require 'inf-ruby nil :noerror)
;;  package-install 'inf-ruby)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)


;; yari: in buffer ruby documentation
;; (when (require 'yari nil :noerror)
;;  package-install 'yari)

(defun ri-bind-key ()
   (local-set-key [f1] 'yari))

(add-hook 'ruby-mode-hook 'ri-bind-key)


;; flycheck: code style checking
;; (when (require 'flycheck nil :noerror)
;;  package-install 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)


;; smartparens: better parenthasis handling & highlighting
;; (when (require 'smartparens nil :noerror)
;;  package-install 'smartparens)

(require 'smartparens-config)

(add-hook 'after-init-hook #'smartparens-global-mode)

(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(add-hook 'prog-mode-hook 'emr-initialize)


(provide '.emacs)
;;; .emacs ends here

