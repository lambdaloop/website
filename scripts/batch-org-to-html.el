;;; Code

;; load files from the directory we're in

(setq dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path dir)
(add-to-list 'load-path "~/builds/org-mode/lisp")
(require 'ox-html)

(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-font-prefix "org-")
(setq org-html-postamble nil)

(setq org-export-with-smart-quotes t
      org-html-validation-link nil
      org-latex-prefer-user-labels t)

(setq org-html-head "
      <link href=\"/css/code-theme.css\" rel=\"stylesheet\">
      <link href=\"/css/style.css\" rel=\"stylesheet\">
      <script src=\"/js/footnotes.js\"></script>
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

(defun batch-org-to-html (org-file)
  "Convert ORG-FILE with a .org extension to FILE.html
Use batch-org-to-html from the command line in batch mode."
  (batch-org-to-html--load-file org-file)
  (org-html-export-to-html))

