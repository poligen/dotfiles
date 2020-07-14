;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! ox-reveal :recipe (:host github :repo "yjwen/org-reveal"))
(package! org-cliplink :recipe (:host github :repo "rexim/org-cliplink"))
(package! org-download :recipe (:host github :repo "abo-abo/org-download"))
(package! writegood-mode :recipe (:host github :repo "bnbeckwith/writegood-mode"))
(package! langtool :recipe (:host github :repo "mhayashi1120/Emacs-langtool"))
(package! org-ref)
(package! rog-ref-ox-hugo :recipe (:host github :repo "jethrokuan/org-ref-ox-hugo" :branch "custom/overrides"))
(package! interleave)
(package! org-noter)
(package! el-patch :recipe (:host github :repo "raxod502/el-patch"))
(package! anki-editor)
(package! nov)
