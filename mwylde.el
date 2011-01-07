;; Speed-bar in frame
(load "~/.emacs.d/mwylde/sr-speedbar.el")
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; tab width
(setq-default tab-width 4)

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
;;(autoload 'js2-mode "js2" nil t)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Set color theme to twilight
(color-theme-twilight)

;; PeepOpen, but only for OS X
(if (eq system-type 'darwin)
  (load "~/.emacs.d/mwylde/peepopen.el")
  (require 'peepopen)
  (textmate-mode)
  (setq ns-pop-up-frames nil))

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
(load "~/.emacs.d/mwylde/smart-tabs.el")
(require 'smarttabs)
(defun smarttabs-enable ()
  "Enables smart tabs"
  (interactive)
  (setq indent-tabs-mode t)
  (smart-tabs-advice ruby-indent-line ruby-indent-level)
  (setq ruby-indent-tabs-mode t))

(defun smarttabs-disable ()
  "Disabled smart tabs"
  (interactive)
  (setq indent-tabs-mode nil)
  (setq ruby-indent-tabs-mode nil)
  (setq ruby-indent-spaces-mode t)
  (setq ruby-indent-level 2))

(smart-tabs-advice js-indent-line js-expr-basic-offset)
;;(setq js-indent-tabs-mode t)

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
