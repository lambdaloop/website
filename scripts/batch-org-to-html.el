;;; Code

;; load files from the directory we're in

(setq dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path dir)
;; (add-to-list 'load-path "~/builds/org-mode/lisp")
(require 'ox-html)

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun concat-path (a b)
  (concat (file-name-as-directory a) b)
  )

(setq templates-dir (concat-path (parent-directory dir) "templates"))


(defun my-org-html-toc-no-heading (args)
  "Avoid toc heading in html export if the keyword TOC_HO_HEADING is t or yes.
Works as a :filter-args advice for `org-html-toc' with argument list ARGS."
  (let* ((depth (nth 0 args))
     (info (nth 1 args))
     (scope (nth 2 args)))
    (when (null scope)
      (setq scope (plist-get info :parse-tree)))
    (list depth info scope)))

(advice-add 'org-html-toc :filter-args #'my-org-html-toc-no-heading)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-font-prefix "org-")
(setq org-html-postamble nil)
(setq org-html-preamble (get-string-from-file (concat-path templates-dir "header.html")))

(setq org-export-with-smart-quotes t
      org-html-validation-link nil
      org-latex-prefer-user-labels t)

(setq org-html-head "
      <link href=\"/css/code-theme.css\" rel=\"stylesheet\">
      <link href=\"/css/style.css\" rel=\"stylesheet\">
      <script src=\"/js/footnotes.js\"></script>
      <link rel=\"stylesheet\"
          href=\"https://fonts.googleapis.com/css?family=Source Sans\">
"
      )

(setq user-full-name "lambdaloop"
      user-mail-address "krchtchk@gmail.com")

(setcdr (assq 'path org-html-mathjax-options)
        '("https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js")
        )

(setq org-html-mathjax-template
      "<script>
window.MathJax = {
  chtml: {
    displayAlign: \"%ALIGN\",
    displayIndent: \"%INDENT\",
    scale: %SCALE / 100
  },
  svg: {
    scale: %SCALE / 100
  },
  tex: {
    tags: \"ams\",
    multlineWidth: \"%MULTLINEWIDTH\",
    tagSide: \"%TAGSIDE\",
    tagIndent: \"%TAGINDENT\",
    autoload: {
      color: [],
      colorV2: ['color']
    },
    packages: {'[+]': ['noerrors']}
  },
  options: {
    ignoreHtmlClass: 'tex2jax_ignore',
    processHtmlClass: 'tex2jax_process'
  },
  loader: {
    load: ['[tex]/noerrors']
  }
};
</script>
<script src=\"%PATH\" id=\"MathJax-script\"></script>"
      )

(defun batch-org-to-html--load-file (org-file)
  (setq make-backup-files nil)   ;; no need to create backup~ on generated files
  (find-file org-file))

;; thanks to:
;; https://archive.casouri.cat/note/2020/org-html-export:-permanent-section-link/index.html
(defun luna-publish-populate-header-id ()
  "Add CUSTOM_ID property to each header in current buffer."
  (let (id-list)
    (cl-labels ((get-id ()
                        (let ((id (url-encode-url
                                   (replace-regexp-in-string
                                    " " "-"
                                    (org-get-heading t t t t))))
                              (dup-counter 1))
                          (while (member id id-list)
                            (setq id (format "%s-%d" id dup-counter))
                            (cl-incf dup-counter))
                          (push id id-list)
                          id)))
      (org-map-entries
       (lambda ()
         (org-entry-put (point) "CUSTOM_ID" (get-id)))))))

(defun batch-org-to-html (org-file)
  "Convert ORG-FILE with a .org extension to FILE.html
Use batch-org-to-html from the command line in batch mode."
  (batch-org-to-html--load-file org-file)
  (luna-publish-populate-header-id)
  (org-html-export-to-html))
