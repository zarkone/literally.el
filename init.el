(let ((gc-cons-threshold most-positive-fixnum))
  ;; Set repositories
  (require 'package)
  (setq-default
   custom-file "~/.emacs.d/.custom-vars" ;; set custom file but never load it; config custom with use-package instead
   load-prefer-newer t
   package-enable-at-startup nil)

  ;; (load custom-file)

  (setq
   package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))

  (package-initialize)

  (require 'use-package-ensure)
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t)

  ;; https://github.com/slotThe/vc-use-package
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package)

  (defun my/org-agenda-show-time-if-any ()
    (let ((scheduled (org-get-scheduled-time (point))))
      (if scheduled
          (format-time-string "⏰%H:%M " scheduled)
        "       "))) ;; 7 spaces to align with time
  (defun my/org-agenda-show-date-if-any ()
    (let ((scheduled (org-get-scheduled-time (point))))
      (if scheduled
          (format "📅%s" (format-time-string "%a" scheduled))
        "☐           "))) ;; Align with Sch:<YYYY-MM-DD>

  ;; Use latest Org
  (use-package org
    :bind (("C-x M-a" . org-agenda)
           :map org-mode-map
           ("<C-return>" . save-buffer))
    :custom
    (org-log-done t)
    (org-agenda-files '("/home/zarkone/docs/notes/anytype/inbox.org"))
    (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "HOLD" "|" "DONE" "CANCELLED")))
    (org-global-properties '(("Effort_ALL". "5m 15m 25m 40m 55m")))
    (org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

    (org-agenda-prefix-format
     '((agenda . " %i %-12:c %(my/org-agenda-show-time-if-any) e:%-6e ")
       (todo   . " %i %-12:c %(my/org-agenda-show-date-if-any)  e:%-6e ")
       (tags   . " %i %-12:c %(my/org-agenda-show-date-if-any)  e:%-6e ")
       (search . " %i %-12:c %(my/org-agenda-show-date-if-any)  e:%-6e ")))


    (org-todo-keyword-faces
     '(("TODO" . org-warning)
       ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
       ("HOLD" . (:foreground "purple" :weight bold))
       ("CANCELLED" . (:foreground "pink"))))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)))
    :config
    (custom-set-faces
     '(org-link ((t (:inherit link :underline t :italic t)))))
    (use-package org-tempo
      :demand t
      :ensure nil))

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "literally.org" user-emacs-directory))
  (garbage-collect))
