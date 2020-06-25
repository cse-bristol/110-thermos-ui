(require 'org)

(defun y-or-n-p (&rest) t)
(setq org-confirm-babel-evaluate nil)

(defun org-latex--skip-video (o link info)
  (let* ((path (let ((raw-path (org-element-property :path link)))
		 (if (not (file-name-absolute-p raw-path)) raw-path
		   (expand-file-name raw-path))))
         (filetype (file-name-extension path)))
    (if (string= filetype "webm")
        ""
      (funcall o link info))))

(advice-add 'org-latex--inline-image :around 'org-latex--skip-video)

(defun org-html--format-video (orig source attributes info)
  (message (file-name-extension source))
  (if (string= "webm" (file-name-extension source))
      (concat
       "<video "
       (org-html--make-attribute-string
         (org-combine-plists
          (list :src source)
          attributes))
       ">"
       (org-html-close-tag
        "source"
        ""
        info)
       "</video>")
    (funcall orig source attributes info)))

(advice-add 'org-html--format-image  :around 'org-html--format-video)

(setq org-html-inline-image-rules
      '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\|webm\\)\\'")
        ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\|webm\\)\\'")
        ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\|webm\\)\\'"))
      )

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

(setq org-confirm-babel-evaluate nil)

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

(when nil
  (let ((org-export-with-date nil)
        (org-latex-packages-alist
         '(("" "fullpage" nil) ("" "parskip" nil))
         )
        ;; need to skip wemb files in pdfs
        (org-publish-project-alist
         `(("docs"
            :time-stamp-file nil
            :base-directory ,default-directory
            :publishing-directory "/home/hinton/tmp/thermos-pdfs"
            :recursive t
            :publishing-function org-latex-publish-to-pdf)
           )))
    (org-publish-project "docs" t))


  (defun org-odt-publish-to-odt (plist filename pub-dir)
    "Publish an org file to ODT.
 
 FILENAME is the filename of the Org file to be published.  PLIST
 is the property list of the given project.  PUB-DIR is the publishing
 directory.
 
 Return output file name."
    (unless (or (not pub-dir) (file-exists-p pub-dir)) (make-directory pub-dir t))
    ;; Check if a buffer visiting FILENAME is already open.
    (let* ((org-inhibit-startup t)
           (visiting (find-buffer-visiting filename))
           (work-buffer (or visiting (find-file-noselect filename))))
      (unwind-protect
          (with-current-buffer work-buffer
            (let ((outfile (org-export-output-file-name ".odt" nil pub-dir)))
              (org-odt--export-wrap
               outfile
               (let* ((org-odt-embedded-images-count 0)
                      (org-odt-embedded-formulas-count 0)
                      (org-odt-object-counters nil)
                      (hfy-user-sheet-assoc nil))
                 (let ((output (org-export-as 'odt nil nil nil
                                              (org-combine-plists
                                               plist
                                               `(:crossrefs
                                                 ,(org-publish-cache-get-file-property
                                                   (expand-file-name filename) :crossrefs nil t)
                                                 :filter-final-output
                                                 (org-publish--store-crossrefs
                                                  org-publish-collect-index
                                                  ,@(plist-get plist :filter-final-output))))))
                       (out-buf (progn (require 'nxml-mode)
                                       (let ((nxml-auto-insert-xml-declaration-flag nil))
                                         (find-file-noselect
                                          (concat org-odt-zip-dir "content.xml") t)))))
                   (with-current-buffer out-buf (erase-buffer) (insert output))))))))
      (unless visiting (kill-buffer work-buffer))))

  (let ((org-export-with-date nil)
        (org-odt-preferred-output-format "docx")
        (org-latex-packages-alist
         '(("" "fullpage" nil) ("" "parskip" nil))
         )
        ;; need to skip wemb files in pdfs
        (org-publish-project-alist
         `(("docs"
            :time-stamp-file nil
            :base-directory ,default-directory
            :publishing-directory "/home/hinton/tmp/thermos-pdfs"
            :recursive t
            :publishing-function org-odt-publish-to-odt)
           )))
    (org-publish-project "docs" t)))
