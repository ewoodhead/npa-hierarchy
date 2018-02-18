(setq-default fill-column 77)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(setq disabled-command-function nil)


;;; Set repositories and start Emacs package system.

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; Check if packages are installed and install them if necessary.

(defun ensure-installed (packages)
  "Install PACKAGES if they aren't installed already."
  (dolist (p packages)
    (unless (package-installed-p p)
      (package-install p))))

(ensure-installed '(slime paredit auto-complete ac-slime))


;;; auto-complete

(ac-config-default)


;;; IELM

(add-hook 'ielm-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-functions
                               ac-source-variables
                               ac-source-features
                               ac-source-symbols
                               ac-source-words-in-same-mode-buffers))
            (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
            (auto-complete-mode 1)))

;; Make <return> only evaluate expression when the cursor is at the end.
(defun ielm-return-at-end ()
  (interactive)
  (if (< (point) (point-max))
      (newline-and-indent)
    (ielm-return)))

(add-hook 'ielm-mode-hook
          (lambda ()
            (define-key ielm-map
              (kbd "<return>")
              #'ielm-return-at-end)))


;;; Default browser.

(setq browse-url-browser-function #'w3m-browse-url)


;;; SLIME

(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl" "--dynamic-space-size" "16384"))))

(add-hook 'lisp-mode-hook
          (lambda ()
            (unless (featurep 'slime)
              (require 'slime) 
              (normal-mode))))

(eval-after-load "slime" '(slime-setup '(slime-fancy slime-banner)))

;; Autocomplete for SLIME
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'slime-repl-mode))

;; Common Lisp Hyperspec lookup
;(load "~/quicklisp/clhs-use-local.el" t)

;; Indent preferences for the Lisp LOOP macro.
(setq lisp-simple-loop-indentation 2
      lisp-loop-keyword-indentation 6
      lisp-loop-forms-indentation 6)

;; Make <return> only evaluate expression when the cursor is at the end. I
;; more or less copied the way it is done in Portacle
;; (https://portacle.github.io/).
(defun slime-repl-return-at-end ()
  (interactive)
  (if (< (point) (point-max))
      (slime-repl-newline-and-indent)
    (slime-repl-return)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map
              (kbd "<return>")
              #'slime-repl-return-at-end)))

;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing
;; over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)


;;; ParEdit

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)

(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
