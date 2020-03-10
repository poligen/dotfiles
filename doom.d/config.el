;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here

;; use =C-h v= to set org variable

(require 'writegood-mode)
(require 'org)

;;20191118 bug
;; Patch up the evil-org key map, so that org is usable with daemon
;; https://github.com/hlissner/doom-emacs/issues/1897
(after! evil-org
  (evil-define-key '(normal visual) evil-org-mode-map
    (kbd "TAB") 'org-cycle))

;; org-attach setting
;;
(require 'org-attach)
(setq org-link-abbrev-alist '(
                              ("att" . org-attach-expand-link)
                              ("download" . "~/Nextcloud/Org/")
                              ))

;; Directory org
(setq  org-directory "~/Nextcloud/Org/"
       org-attach-id-dir "~/Nextcloud/Org/data/"
      )


;; three tools for better english writing:
;; write-good setting mode
;; flycheck with proselint(python)
;; langtool-check(java with languagetool) for the english seplling
;; Set a global key to toggle the mode
(global-set-key "\C-cg" 'writegood-mode)

;; setup languagetool
;; (setq langtool-bin "/usr/local/bin/languagetool")
;; (require 'langtool)

;; (global-set-key "\C-x4w" 'langtool-check)
;; (global-set-key "\C-x4W" 'langtool-check-done)
;; (global-set-key "\C-x4l" 'langtool-switch-default-language)
;; (global-set-key "\C-x44" 'langtool-show-message-at-point)
;; (global-set-key "\C-x4c" 'langtool-correct-buffer)



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
(global-set-key (kbd "C-x C-i") 'org-cliplink)

;; org-download setting
(require 'org-download)
 ;; org-download use buffer-local variables. Set it individually in files. Otherwise, put things flatly in misc
  ;; folder.
(setq-default   org-download-method 'attach
                org-download-heading-lvl nil
                org-download-delete-image-after-download t
                org-download-screenshot-method "echo"
                org-download-image-dir "~/Nextcloud/Org"
                org-download-screenshot-file "/tmp/screenshot.png"
                org-download-image-org-width 800
                org-download-annotate-function '(lambda (_link) "")
                )

  ;; My customized org-download to incorporate flameshot gui Workaround to setup flameshot, which enables annotation.
  ;; In flameshot, set filename as "screenshot", and the command as "flameshot gui -p /tmp", so that we always ends up
  ;; with /tmp/screenshot.png. Nullify org-download-screenshot-method by setting it to `echo', so that essentially we
  ;; are only calling (org-download-image org-download-screenshot-file).
  (defun my-org-download-screenshot ()
    "Capture screenshot and insert the resulting file.
The screenshot tool is determined by `org-download-screenshot-method'."
    (interactive)
    (let ((tmp-file "/tmp/screenshot.png"))
      (delete-file tmp-file)
      (call-process-shell-command "flameshot gui -p /tmp/")
      ;; Because flameshot exit immediately, keep polling to check file existence
      (while (not (file-exists-p tmp-file))
        (sleep-for 2))
      (org-download-image tmp-file)))

  (global-set-key (kbd "<print>") 'my-org-download-screenshot)
  ;; Use #+ATTR_ORG: :width 300px to customized image display width
  (setq org-image-actual-width nil)
  ;; display inline
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")



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
       (("~/Nextcloud/Org/someday.org" :maxlevel . 3)
        ("~/Nextcloud/Org/inbox.org" :maxlevel . 3)
        ("~/Nextcloud/Org/books.org" :maxlevel . 3)
        ("~/Nextcloud/Org/mylife.org" :maxlevel . 3))))

    ;; org-capture
    ;; (setq org-default-notes-file custom-org-mode-capture-file)
    ;; for org-protocol use
    (defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
  )

    (setq org-capture-templates
          (quote
           (("p" "Private Templates")
            ("pt" "add todo" entry
             (file+headline "~/Nextcloud/Org/inbox.org" "Tasks")
             (file "~/Nextcloud/dotfiles/templates/tl-todo.txt")
             :empty-lines 1)
            ("pj" "my personal journal" entry
             (file+olp+datetree "~/Nextcloud/Org/journal.org")
             "** %U -- %^{Heading}
 %?" :empty-lines 1)
            ("pn" "quick notes" entry
             (file+headline "~/Nextcloud/Org/inbox.org" "Notes")
             (file "~/Nextcloud/dotfiles/templates/tl-todo.txt"))
            ("ps" "Someday entry" entry
             (file+headline "~/Nextcloud/Org/someday.org" "Someday")
             "** %^{description} - %u %^g
 %?")
            ("pb" "Book I want to read" entry
             (file+headline "~/Nextcloud/Org/books.org" "Books I want to read")
             (file "~/Nextcloud/dotfiles/templates/tl-book.txt"))
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
            ;; Org-capture templates
            ("a" "Anki Templates")
            ("ab" "Anki basic"
             entry
             (file+headline "~/Nextcloud/Org/anki.org" "Shelf")
             "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic-orgmode\n:ANKI_DECK: Big\n:END:\n** Front\n%?\n** Back\n%x\n")
            ("ac" "Anki cloze"
             entry
             (file+headline "~/Nextcloud/Org/anki.org" "Shelf")
             "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Big\n:END:\n** Text\n%x\n** Extra\n")
            ))))




;; org-tag-align
(add-hook 'focus-in-hook
  (lambda () (progn
    (setq org-tags-column (- 5 (window-body-width)))) (org-align-all-tags)))

(add-hook 'focus-out-hook
  (lambda () (progn
    (setq org-tags-column (- 5 (window-body-width)))) (org-align-all-tags)))



;; Automating adding the ID
;; https://writequit.org/articles/emacs-org-mode-generate-ids.html
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(defun eos/org-custom-id-get (&optional pom create prefix)
  "Get the ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an ID, the function returns nil. However, when
   CREATE is non nil, create a ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new))
        (org-entry-put pom "ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun eos/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current
   file which do not already have one. Only adds ids if the
   `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
      (org-map-entries (lambda () (eos/org-custom-id-get (point) 'create))))))



(defun org-id-new (&optional prefix)
  "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.

PREFIX can specify the prefix, the default is given by the variable
`org-id-prefix'.  However, if PREFIX is the symbol `none', don't use any
prefix even if `org-id-prefix' specifies one.

So a typical ID could look like \"Org-4nd91V40HI\"."
  (let* ((prefix (if (eq prefix 'none)
                     ""
                   (concat (or prefix org-id-prefix) "-")))
         unique)
    (if (equal prefix "-") (setq prefix ""))
    (cond
     ((memq org-id-method '(uuidgen uuid))
      (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
      (unless (org-uuidgen-p unique)
        (setq unique (org-id-uuid))))
     ((eq org-id-method 'org)
      (let* ((etime (org-reverse-string (org-id-time-to-b36)))
             (postfix (if org-id-include-domain
                          (progn
                            (require 'message)
                            (concat "@" (message-make-fqdn))))))
        (setq unique (concat etime postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix unique)))



;; automatically add ids to captured headlines
(add-hook 'org-capture-prepare-finalize-hook
          (lambda () (eos/org-custom-id-get (point) 'create)))

;; automatically add ids to saved org-mode headlines
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (and (eq major-mode 'org-mode)
                                   (eq buffer-read-only nil))
                          (eos/org-add-ids-to-headlines-in-file))))))



;; anki-editor setting
(use-package anki-editor
  :after org
  :bind (:map org-mode-map
              ("C-c a a" . anki-editor-cloze-region-auto-incr)
              ("C-c a n" . anki-editor-cloze-region-dont-incr)
              ("C-c a r" . anki-editor-reset-cloze-number)
              ("C-c a p"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number)
  )
  ;; Allow Emacs to access content from clipboard.
(setq select-enable-clipboard t
      select-enable-primary t)



(eval-when-compile
  (require 'el-patch))

(use-package deft
  :after org
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Nextcloud/org-roam/")
  )


;; org-roam setting
(setq org-roam-link-title-format "R:%s")
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init 
  (setq org-roam-directory "~/Nextcloud/org-roam/")
  (map! :leader 
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-insert
        :desc "Org-Roam-Find"   "/" #'org-roam-find-file
        :desc "Org-Roam-Buffer" "r" #'org-roam)
  :config
  (org-roam-mode +1))

;; org-roam template
(setq org-roam-capture-templates
      '(("h" "hugo blogging" plain
       (function org-roam--capture-get-point)
       "%?"
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :head "#+HUGO_SECTION: post
#+HUGO_BASE_DIR: ~/Nextcloud/my-blogs/anatomind/
#+HUGO_TAGS: %^{Tags}

#+EXPORT_FILE_NAME: %^{export name}
#+TITLE: ${title}
#+AUTHOR: Suan-sing Tan
#+DATE: %t"
       :unnarrowed t)
      ("d" "default" plain
       (function org-roam--capture-get-point)
       "%?"
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :head "#+TITLE: ${title}\n#+ROAM_KEY: \n#+ROAM_ALIAS: \n - tags :: \n"
       :unnarrowed t))
      )
;; org-roam-protocol
(require 'org-roam-protocol)


;;org-journal setting
(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Nextcloud/org-roam/")
  (org-journal-date-format "%A, %d %B %Y"))

;;nov setting to read epub
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


;;org-ref setting
(require 'org-ref)
(setq reftex-default-bibliography '("~/Nextcloud/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Nextcloud/bibliography/notes.org"
      org-ref-default-bibliography '("~/Nextcloud/bibliography/references.bib")
      org-ref-pdf-directory "~/Nextcloud/bibliography/bibtex-pdfs/")
;;org-ref-ox-hugo
(use-package org-ref-ox-hugo
  :after org org-ref ox-hugo
  :config
  (add-to-list 'org-ref-formatted-citation-formats
               '("md"
                 ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
                 ("inproceedings" . "${author}, *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("book" . "${author}, *${title}* (${year}), ${address}: ${publisher}.")
                 ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
                 ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
                 ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
                 ("misc" . "${author} (${year}). *${title}*. Retrieved from [${howpublished}](${howpublished}). ${note}.")
                 (nil . "${author}, *${title}* (${year})."))))
