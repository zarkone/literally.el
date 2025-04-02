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

  (defun my/org-agenda-insert-manual-clock ()
    "Insert a manual CLOCK entry under the current Org heading.
Can be run from org-agenda or org buffer."
    (interactive)
    ;; Jump to Org entry if in Agenda view
    (when (equal major-mode 'org-agenda-mode)
      (org-agenda-goto))
    (let* ((start (org-read-date t t nil "Start time: "))
           (end (org-read-date t t nil "End time: "))
           (duration (org-duration-from-minutes
                      (/ (float-time (time-subtract end start)) 60))))
      (save-excursion
        (org-back-to-heading t)
        (org-end-of-meta-data)
        (let ((logbook-start (re-search-forward ":LOGBOOK:" (save-excursion (org-end-of-subtree t t)) t))
              (logbook-end))
          ;; Create LOGBOOK drawer if it doesn't exist
          (unless logbook-start
            (insert ":LOGBOOK:\n:END:\n")
            (forward-line -1)
            (setq logbook-start (point)))
          ;; Insert CLOCK line just before :END:
          (goto-char logbook-start)
          (setq logbook-end (re-search-forward ":END:" nil t))
          (goto-char (or logbook-end (point-max)))
          (beginning-of-line)
          (insert (format "CLOCK: [%s]--[%s] =>  %s\n"
                          (format-time-string "%Y-%m-%d %a %H:%M" start)
                          (format-time-string "%Y-%m-%d %a %H:%M" end)
                          duration))))))

  (defun my/org-format-clocksum-minutes ()
    "Return the clocksum of the current entry as e.g. '1h 15m'."
    (let* ((clocksum (org-entry-get nil "CLOCKSUM"))
           (parts (when clocksum (split-string clocksum ":")))
           (hours (string-to-number (or (car parts) "0")))
           (minutes (string-to-number (or (cadr parts) "0")))
           (total-minutes (+ (* hours 60) minutes)))
      (if (= total-minutes 0) "      "
        (cond
         ((< total-minutes 60) (format "/%2dm  " total-minutes))
         ((= (mod total-minutes 60) 0) (format "/%dh   " (/ total-minutes 60)))
         (t (format "/%dh%2dm" (/ total-minutes 60) (mod total-minutes 60)))))
      ))

  (defun my/org-agenda-show-time-if-any ()
    (let ((scheduled (org-get-scheduled-time (point))))
      (if scheduled
          (format-time-string "📅%H:%M " scheduled)
        "        ")))

  (defun my/org-agenda-show-date-if-any ()
    (let ((scheduled (org-get-scheduled-time (point))))
      (if scheduled
          (format "📅%s" (format-time-string "%a" scheduled))
        "·····")))

  ;; Use latest Org
  (use-package org
    :hook
    ((org-agenda-mode . (lambda ()
                          (define-key org-agenda-mode-map (kbd "M") #'my/org-agenda-insert-manual-clock))))
    :bind (("C-x M-a" . org-agenda)
           :map org-mode-map
           ("<C-return>" . save-buffer))
    :custom
    (org-agenda-sticky nil)
    (org-log-done t)
    (org-agenda-files '("/home/zarkone/docs/notes/anytype/inbox.org"))
    (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "NEXT" "HOLD" "|" "DONE" "CANCELLED")))
    (org-global-properties '(("Effort_ALL". "5m 15m 25m 40m 55m")))
    (org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
    (org-startup-folded 'overview)
    (org-hide-properties t)
    (org-agenda-start-day "-7d")
    (org-agenda-span 21)

    (org-agenda-prefix-format
     '((agenda  . " %i %-12:c%?-12t%-3e%(my/org-format-clocksum-minutes) ")
       ;;(agenda . " %i %-12:c %(my/org-agenda-show-time-if-any) %(my/org-agenda-add-clocked-time) e:%-6e ")
       (todo   . " %i %-12:c %(my/org-agenda-show-date-if-any)  e:%-6e ")
       (tags   . " %i %-12:c %(my/org-agenda-show-date-if-any)  e:%-6e ")
       (search . " %i %-12:c %(my/org-agenda-show-date-if-any)  e:%-6e ")))

    (org-agenda-custom-commands '(("w" "Anytype tasks"
                                   ((agenda "")
                                    (alltodo ""))
                                   ((org-agenda-files '("/home/zarkone/docs/notes/anytype/inbox.org"))
                                    (org-agenda-span 14)))
                                  ("b" " Business"
                                   ((agenda "")
                                    (alltodo ""))
                                   ((org-agenda-files '("/home/zarkone/docs/notes/business.org"
                                                        "/home/zarkone/docs/notes/pasm"
                                                        "/home/zarkone/docs/notes/routine.org"))))
                                  ("m" " My"
                                   ((agenda "")
                                    (alltodo ""))
                                   ((org-agenda-files '("/home/zarkone/docs/notes/my.org"
                                                        "/home/zarkone/docs/notes/routine.org"))))))



    (org-todo-keyword-faces
     '(("TODO" . org-warning)
       ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
       ("NEXT" . (:foreground "lavander" :weight bold))
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
