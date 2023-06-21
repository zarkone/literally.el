(let ((gc-cons-threshold most-positive-fixnum))
  ;; Set repositories
  (require 'package)
  (setq-default
   custom-file "~/.emacs.d/.custom-vars" ;; set custom file but never load it; config custom with use-package instead
   load-prefer-newer t
   package-enable-at-startup nil)

  (setq
   package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ;;("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "http://melpa.org/packages/")))
  ;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; MacOS package not found workaround
  (when (eq system-type 'darwin)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

  (unless (bound-and-true-p package--initialized)
    (setq package-enable-at-startup nil) ; To prevent initializing twice
    (package-initialize))

  ;; Install dependencies
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package t))
  (eval-when-compile
    (require 'use-package))
  (setq-default
   use-package-always-defer t
   use-package-always-ensure t)

  ;; https://github.com/slotThe/vc-use-package
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package)

  ;; Use latest Org
  (use-package org
    :bind (("C-x M-a" . org-agenda)
           :map org-mode-map
           ("<C-return>" . save-buffer))
    :custom
    (org-log-done t)
    (org-agenda-files '("/home/zarkone/work/pitch/notes"))
    (org-todo-keywords '((sequence "TODO" "HOLD" "|" "DONE" "CANCELLED")))
    (org-todo-keyword-faces
     '(("TODO" . org-warning)
       ("HOLD" . (:foreground "purple" :weight bold))
       ("CANCELLED" . (:foreground "pink"))))
    :config
    (custom-set-faces
     '(org-link ((t (:inherit link :underline t :italic t)))))
    (use-package org-tempo
      :demand t
      :ensure nil))

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "literally.org" user-emacs-directory))
  (garbage-collect))
