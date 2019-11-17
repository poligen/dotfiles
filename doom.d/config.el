;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;
;;

(require 'writegood-mode)
(require 'org)


;; Directory org
(setq  org-directory "~/Nextcloud/Org/"
       org-attach-id-dir "~/Nextcloud/Org/data/"
       org-attach-directory "~/Nextcloud/Org/data/"
      )

;; three tools for better english writing:
;; write-good mode setting
;; flycheck with proselint(python)
;; langtool-check(java with languagetool) for the english seplling
;; Set a global key to toggle the mode
(global-set-key "\C-cg" 'writegood-mode)

;; setup languagetool
(setq langtool-bin "/usr/local/bin/languagetool")
(require 'langtool)

(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)



;; use treemacs-evil mode
(use-package! treemacs-evil
  ;; :when (featurep! :editor evil +everywhere)
  :after treemacs
  :config
  (define-key! evil-treemacs-state-map
    [return] #'treemacs-RET-action
    [tab]    #'treemacs-TAB-action
    "TAB"    #'treemacs-TAB-action
    ;; REVIEW Fix #1875 to be consistent with C-w {v,s}, but this should really
    ;;        be considered upstream.
    "o v"    #'treemacs-visit-node-horizontal-split
    "o s"    #'treemacs-visit-node-vertical-split))

;;Auto adjust default window frame size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)


;;auto show company mode, if you don't want auto-compete, delete the code
(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

;; custom set varibable
'(org-log-into-drawer t)
'(org-log-reschedule (quote note))
'(org-modules
  (quote
   (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-protocol org-rmail org-w3m)))
'(org-refile-allow-creating-parent-nodes (quote confirm))
'(org-refile-use-outline-path (quote file))


(setq doom-font (font-spec :family "Fira Code" :size 18 :powerline-scale 1.2))
;; font for chinese word
(if window-system
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset (font-spec :family "WenQuanYi Zen Hei Medium" :size 18))))



;; easy-template after 9.2
(require 'org-tempo)

;; latex-preview
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; large latex preview
(plist-put org-format-latex-options :scale 2)


;; org-cliplink
;;(global-set-key (kbd "C-x p i") 'org-cliplink)

;; org-download setting
(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)


;; org-image-width setting
(setq org-image-actual-width nil)
;; org-download-default-width
(setq org-download-image-org-width 600)



  ;; org-image-width setting
  (setq org-image-actual-width nil)
  ;; org-download-default-width
  (setq org-download-image-org-width 600)

;; org all setting start here
  ;; start org-protocol
  (require 'org-protocol)

;; with eval after load org
  (with-eval-after-load 'org
    ;; org-protocol
    (add-to-list 'org-modules 'org-protocol)


    ;; org-agenda
    (setq org-agenda-files (list "~/Nextcloud/Org/inbox.org"
                                 "~/Nextcloud/Org/mylife.org"
                                 "~/Nextcloud/Org/someday.org"))


    ;; org-refile
    (setq org-refile-targets
      (quote
       (("someday.org" :maxlevel . 3)
        ("inbox.org" :maxlevel . 3)
        ("note.org" :maxlevel . 3)
        ("mylife.org" :maxlevel . 3))))

    ;; org-capture
    ;; (setq org-default-notes-file custom-org-mode-capture-file)
    (setq org-capture-templates
          (quote
           (("p" "Private Templates")
            ("pt" "add todo" entry
             (file+headline "~/Nextcloud/Org/inbox.org" "Tasks")
             (file "~/dotfiles/templates/tl-todo.txt")
             :empty-lines 1)
            ("pj" "my personal journal" entry
             (file+olp+datetree "~/Nextcloud/Org/journal.org")
             "** %U -- %^{Heading}
 %?" :empty-lines 1)
            ("pn" "quick notes" entry
             (file+headline "~/Nextcloud/Org/inbox.org" "Notes")
             (file "~/dotfiles/templates/tl-todo.txt"))
            ("ps" "Someday entry" entry
             (file+headline "~/Nextcloud/Org/someday.org" "Someday")
             "** %^{description} - %u %^g
 %?")
            ("pb" "Book I want to read" entry
             (file+headline "~/Nextcloud/Org/mylife.org" "Books to read")
             (file "~/dotfiles/templates/tl-book.txt"))
            ("pr" "my running record" table-line
             (file+headline "~/Nextcloud/Org/mylife.org" "Jogging")
             "| %u | %^{km} | %^{duration(HH:MM:SS)} |" :kill-buffer t)
            ("pv" "movie or tv show" entry
             (file+headline "~/Nextcloud/Org/mylife.org" "Films")
             "** %^{Name} - %u")
            ("w" "Working Templates")
            ("wd" "setting this months duty" entry
             (file+olp "~/Nextcloud/Org/mylife.org" "Work" "Duty")
             "** TODO %^t - Dutyday")
            ("wh" "setting homevisit this months" entry
             (file+olp "~/Nextcloud/Org/mylife.org" "Work" "Home Visit")
             "** TODO %^T - Home visit")

            ("c" "Protocol extension" entry
             (file+headline "~/Nextcloud/Org/note.org" "Readlater")
             "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
             )
            ("L" "Protocol Link" entry
             (file+headline "~/Nextcloud/Org/note.org" "Readlater")
             "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
             :empty-lines 1)
            )))
    )
