(let ((gc-cons-threshold most-positive-fixnum))

  ;; Set repositories
  (require 'package)
  (setq-default
   custom-file "/dev/null" ;; set custom file but never load it; config custom with use-package instead
   load-prefer-newer t
   package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize)

  ;; Install dependencies
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package t))
  (setq-default
   use-package-always-defer t
   use-package-always-ensure t)

  ;; Use latest Org
  (use-package org :ensure org-plus-contrib
    :bind (:map org-mode-map
                ("<C-return>" . save-buffer))
    :config
    (use-package org-tempo
      :demand t
      :ensure nil))

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "literally.org" user-emacs-directory))
  (garbage-collect))
