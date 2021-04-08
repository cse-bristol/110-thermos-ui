;; elisp to produce docs from org files.
;; if you want to escape from emacs you can probably use pandoc to go to another format.

;; we need org to export
(require 'org)

;; we want to run wtihout any questions
(defun y-or-n-p (&rest) t)
(setq org-confirm-babel-evaluate nil)

;; we want a new link type which is not interpreted in any way
(org-link-set-parameters "bare"
 :follow 'my-org-follow-bare-link
 :export 'my-org-export-bare-link)

(defvar my-org-follow-bare-link-base-url nil
  "Base URL to be used by `my-org-follow-bare-link'.
If nil, \"bare:\" links cannot be followed.")

(defun my-org-follow-bare-link (path)
  "Follow a \"bare:\" link, or not.
Link PATH is the sole argument."
  (if my-org-follow-bare-link-base-url
      (browse-url (concat my-org-follow-bare-link-base-url path))
    (message "Cannot follow \"bare:\" link: %s" path)))

(defun my-org-export-bare-link (path description backend)
  "Export a \"bare:\" link.
Link PATH, DESCRIPTION, and export BACKEND are expected as
arguments. Only HTML export is supported. Parent paragraph export
attributes are not supported \(see `org-export-read-attribute')."
  (cond
   ((eq backend 'html)
    (let ((path (org-link-unescape path)))
      (format "<a href=\"%s\">%s</a>"
              (url-encode-url path)
              (org-html-encode-plain-text
               (or description path)))))))

;; do output to html!

(let ((org-html-preamble nil)
      (org-html-postamble nil)
      (org-html-head-include-default-style nil)
      (org-html-head-include-scripts nil)
      (org-export-with-date nil)
      
      (org-publish-project-alist
       `(("docs-org"
          :time-stamp-file nil
          :base-directory ,default-directory
          :publishing-directory ,(concat default-directory "../resources/help/")
          :recursive t
          :publishing-function org-html-publish-to-html)
         ("docs-img"
          :base-directory ,default-directory
          :publishing-directory ,(concat default-directory "../resources/public/help/")
          :publishing-function org-publish-attachment
          :base-extension "png\\|webm\\|svg"
          :recursive t)
         ("docs" :components ("docs-org" "docs-img"))
         )))
  (org-publish-project "docs" t))

(kill-emacs)
