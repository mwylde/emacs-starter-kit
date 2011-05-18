;; Adding stuff to path because shell path doesn't get loaded in OS X
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:/Applications/Stata10/StataSE.app/Contents/MacOS:~/.rvm/bin:" (getenv "PATH")))

;; Updated version of color-theme
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path (concat user-specific-dir "/color-theme"))
(require 'color-theme)


;; LaTeX stuff
(custom-set-variables
 '(LaTeX-command "latex -synctex=1")
 '(TeX-view-program-list (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b") ("Preview" "open -a Preview.app %o"))))
 )
(setq TeX-source-correlate-method 'synctex)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

;; Speed-bar in frame
(load "~/.emacs.d/mwylde/sr-speedbar.el")
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; tab width
(setq-default tab-width 2)

;;
(setq-default python-indent 2)

;; Erlang
(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.6.2/emacs/"
                       load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(if (file-directory-p "/usr/local/lib/erlang/lib/tools-2.6.6.2/emacs")
    (require 'erlang-start))


;; commands to simplify setting marks and returning to them
;; courtesy of http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)


;; HAML and SASS modes
(load "~/.emacs.d/mwylde/haml-mode/haml-mode.el")
(load "~/.emacs.d/mwylde/sass-mode/sass-mode.el")
(require 'haml-mode)
(require 'sass-mode)

;; Coffee-script support
(add-to-list 'load-path "~/.emacs.d/mwylde/coffee-mode")
(require 'coffee-mode)

;; Haskell stuff
(load "~/.emacs.d/mwylde/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; JS2, instead of the built-in js-mode
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;; Set color theme to twilight
;;(color-theme-twilight)

;; Solarized color scheme
(add-to-list 'load-path "~/.emacs.d/mwylde/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;; Set default transport for tramp mode to ssh
(setq tramp-default-method "ssh")

;; PeepOpen, but only for OS X
(load "~/.emacs.d/mwylde/peepopen.el")
(require 'peepopen)
(setq ns-pop-up-frames nil)

(defun open (project) (interactive (list (read-directory-name "Peepopen for project: " "~/git/")))
  (flet ((textmate-project-root () (file-truename project)))
    (peepopen-goto-file-gui)))

(global-set-key [(meta ?o)] 'open)

;; Full ack
;;(add-to-list 'load-path "~/.emacs.d/mwylde")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
;;(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)


;; Smart tabs (see http://www.emacswiki.org/emacs/SmartTabs)
;; Basically, use tabs for indentation, spaces for alignment
;; (i.e., the holy grail)
;; (load "~/.emacs.d/mwylde/smart-tabs.el")
;; (require 'smarttabs)
;; (defun smarttabs-enable ()
;;   "Enables smart tabs"
;;   (interactive)
;;   (setq indent-tabs-mode t)
;;   (smart-tabs-advice ruby-indent-line ruby-indent-level)
;;   (setq ruby-indent-tabs-mode t))

;; (defun smarttabs-disable ()
;;   "Disabled smart tabs"
;;   (interactive)
;;   (setq indent-tabs-mode nil)
;;   (setq ruby-indent-tabs-mode nil)
;;   (setq ruby-indent-spaces-mode t)
;;   (setq ruby-indent-level 2))

;; (smart-tabs-advice js-indent-line js-expr-basic-offset)
;;(setq js-indent-tabs-mode t)

;; Automatic indentation of pasted code, ala TextMate
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                                     ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))


;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
  (message "Renamed to %s." new-name)))

;;; bind RET to py-newline-and-indent
(add-hook 'python-mode-hook '(lambda () 
                               (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; ESS: Emacs speaks statistics
(load "~/.emacs.d/mwylde/ess/lisp/ess-site")
(require 'ess-site)

;; SML
(load "~/.emacs.d/mwylde/sml-mode/sml-mode-startup.el")

;; RSpec mode
(require 'rspec-mode)

;; Jabber
(setq load-path (cons  "~/.emacs.d/mwylde/jabber" load-path))
(require 'jabber)
(setq jabber-account-list
      '(("wyldeone@gmail.com" 
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))
;; get rid of the giant avatar icons in chat windows
(setq jabber-chat-buffer-show-avatar nil)

;; Get rid of current-line highlighting
(remove-hook 'coding-hook 'turn-on-hl-line-mode)

;; Automatically reverts unmodifed buffers that have changed on the
;; filesystem. Useful for working with git.
(global-auto-revert-mode)

;; Better c style
(setq c-default-style "linux")
