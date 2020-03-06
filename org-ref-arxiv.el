;;; org-ref-arxiv.el --- arxiv utilities for org-mode        -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; this library creates a new org-link for Arxiv (http://arxiv.org/) entries,
;; and provides functions to retrieve bibtex entries from an Arxiv number.
;;
;; An Arxiv number might look like: cond-mat/0410285 or 1503.01742

;;; Code:
(require 'bibtex)
(require 'dash)
(require 'f)
(require 'org)
(require 's)
(require 'org-ref-utils)

;; This is a local variable defined in `url-http'.  We need it to avoid
;; byte-compiler errors.
(defvar url-http-end-of-headers)
(defvar org-ref-default-bibliography)
(defvar org-ref-pdf-directory)

(declare-function parsebib-find-bibtex-dialect "parsebib")
;; this is a C function
(declare-function libxml-parse-xml-region "xml")


;;
;; Ripped functions from org-ref-bibtex
;;

(defcustom bibtex-sort-order
  '(("article"  . ("author" "title" "journal" "volume" "number" "pages" "year" "doi" "url"))
    ("inproceedings" . ("author" "title" "booktitle" "year" "volume" "number" "pages" "doi" "url"))
    ("book" . ("author" "title" "year" "publisher" "url")))
  "A-list of bibtex entry fields and the order to sort an entry with.
\(entry-type . (list of fields). This is used in
`org-ref-sort-bibtex-entry'. Entry types not listed here will
have fields sorted alphabetically."
  :type '(alist :key-type (string) :value-type (repeat string)))
;;; ~/.dotfiles/emacs/.doom.d/+bibtex-format.el -*- lexical-binding: t; -*-

(defcustom org-ref-clean-bibtex-key-function
  (lambda (key)
    (replace-regexp-in-string ":" "" key))
  "Function to modify a bibtex key.
The default behavior is to remove : from the key."
  :type 'function)

;; downcase entries
;;;###autoload
(defun org-ref-downcase-bibtex-entry ()
  "Downcase the entry type and fields."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (entry-fields)
         (type (downcase (cdr (assoc "=type=" entry))))
         (key (cdr (assoc "=key=" entry))))

    (setq entry-fields (mapcar (lambda (x) (car x)) entry))
    ;; we do not want to reenter these fields
    (setq entry-fields (remove "=key=" entry-fields))
    (setq entry-fields (remove "=type=" entry-fields))

    (bibtex-kill-entry)
    (insert
     (concat "@" (downcase type) "{" key ",\n"
      (mapconcat
       (lambda (field)
  (format "%s = %s,"
   (downcase field)
   (cdr (assoc field entry))))
       entry-fields "\n")
      "\n}\n\n"))
    (bibtex-find-entry key)
    (bibtex-fill-entry)
    (bibtex-clean-entry)))


;;** Clean a bibtex entry
;; These functions operate on a bibtex entry and "clean" it in some way.

(defun orcb-clean-nil (arg)
  "Remove nil from some article fields.
The removal is conditional. Sometimes it is useful to have nil
around, e.g. for ASAP articles where the fields are not defined
yet but will be in the future.
With \\[univeral-argument], run `bibtex-clean-entry' after.
"
  (interactive "P")
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (type (downcase (cdr (assoc "=type=" entry)))))
    (when (string= type "article")
      (cond
       ;; we have volume and pages but number is nil.
       ;; remove the number field.
       ((and (string= type "article")
      (not (string= (cdr (assoc "volume" entry)) "{nil}"))
      (not (string= (cdr (assoc "pages" entry)) "{nil}"))
      (string= (cdr (assoc "number" entry)) "{nil}"))
 (bibtex-set-field "number" "")
 (if arg
            (bibtex-clean-entry)))))))


(defun orcb-clean-nil-opinionated ()
  "Remove nil from all article fields.
Note that by default, this will leave the entry empty, which may
then get deleted by `bibtex-clean-entry.' To disable this
behavior, remove opts-or-alts from `bibtex-entry-format'. This
will leave the empty entries so that you may fill them in later."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (type (downcase (cdr (assoc "=type=" entry)))))
    (when (string= type "article")
      (cl-loop for (field . text) in entry do
               (if (string= text "{nil}")
                   (bibtex-set-field field ""))))))


(defun orcb-clean-doi ()
  "Remove http://dx.doi.org/ in the doi field."
  (let ((doi (bibtex-autokey-get-field "doi")))
    (when (string-match "^http://dx.doi.org/" doi)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "doi" t))))
      (bibtex-kill-field)
      (bibtex-make-field "doi")
      (backward-char)
      (insert (replace-regexp-in-string "^http://dx.doi.org/" "" doi)))))


(defun orcb-clean-year (&optional new-year)
  "Fix years set to 0.
If optional NEW-YEAR set it to that, otherwise prompt for it."
  ;; asap articles often set year to 0, which messes up key
  ;; generation. fix that.
  (let ((year (bibtex-autokey-get-field "year")))
    (when (string= "0" year)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "year" t))))
      (bibtex-kill-field)
      (bibtex-make-field "year")
      (backward-char)
      (insert (or new-year (read-string "Enter year: "))))))


(defun orcb-clean-pages ()
  "Check for empty pages, and put eid in its place if it exists."
  (let ((pages (bibtex-autokey-get-field "pages"))
 (eid (bibtex-autokey-get-field "eid")))
    (when (and (not (string= "" eid))
        (or (string= "" pages)))
      (bibtex-set-field "pages" eid))))


(defun orcb-& ()
  "Replace naked & with \& in a bibtex entry."
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (while (re-search-forward " & " nil t)
      (replace-match " \\\\& "))))


(defun orcb-% ()
  "Replace naked % with % in a bibtex entry."
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (while (re-search-forward "%" nil t)
      (replace-match " \\\\%"))))


(defun orcb-key-comma ()
  "Make sure there is a comma at the end of the first line."
  (bibtex-beginning-of-entry)
  (end-of-line)
  ;; some entries do not have a key or comma in first line. We check and add it,
  ;; if needed.
  (unless (string-match ", *$" (thing-at-point 'line))
    (end-of-line)
    (insert ",")))


(defun orcb-key (&optional allow-duplicate-keys)
  "Replace the key in the entry.
Prompts for replacement if the new key duplicates one already in
the file, unless ALLOW-DUPLICATE-KEYS is non-nil."
  (let ((key (funcall org-ref-clean-bibtex-key-function
        (bibtex-generate-autokey))))
    ;; remove any \\ in the key
    (setq key (replace-regexp-in-string "\\\\" "" key))
    ;; first we delete the existing key
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
 (delete-region (match-beginning bibtex-key-in-head)
         (match-end bibtex-key-in-head)))
    ;; check if the key is in the buffer
    (when (and (not allow-duplicate-keys)
               (save-excursion
                 (bibtex-search-entry key)))
      (save-excursion
 (bibtex-search-entry key)
 (bibtex-copy-entry-as-kill)
 (switch-to-buffer-other-window "*duplicate entry*")
 (bibtex-yank))
      (setq key (bibtex-read-key "Duplicate Key found, edit: " key)))

    (insert key)
    (kill-new key)))



(defcustom org-ref-clean-bibtex-entry-hook
   '(orcb-key-comma
    orcb-&
    orcb-%
    orcb-clean-year
    orcb-key
    orcb-clean-doi
    orcb-clean-pages)
  "Hook that is run in `org-ref-clean-bibtex-entry'.
The functions should have no arguments, and
operate on the bibtex entry at point. You can assume point starts
at the beginning of the entry. These functions are wrapped in
`save-restriction' and `save-excursion' so you do not need to
save the point position.
Org ref contains some functions that are not included by default
such as `orcb-clean-nil' or `orcb-clean-nil-opinionated' that
users may be interested in adding themselves."
  :type 'hook)

;;;###autoload
(defun org-ref-clean-bibtex-entry ()
  "Clean and replace the key in a bibtex entry.
See functions in `org-ref-clean-bibtex-entry-hook'."
  (interactive)
  (save-excursion
    (save-restriction
      (bibtex-narrow-to-entry)
      (bibtex-beginning-of-entry)
      ;; run hooks. each of these operates on the entry with no arguments.
      ;; this did not work like  i thought, it gives a symbolp error.
      ;; (run-hooks org-ref-clean-bibtex-entry-hook)
      (mapc (lambda (x)
       (save-restriction
  (save-excursion
    (funcall x))))
     org-ref-clean-bibtex-entry-hook))))
(defun clean-and-sort-bibtex-entry (&optional allow-duplicate-keys)
  "Sort fields of entry in standard order."
  (interactive)

 "Replace naked % with % in a bibtex entry."
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (while (re-search-forward "%" nil t)
      (replace-match " \\\\%")))

 "Replace naked & with \& in a bibtex entry."
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (while (re-search-forward " & " nil t)
      (replace-match " \\\\& ")))

  "Fix years set to 0.
If optional NEW-YEAR set it to that, otherwise prompt for it."
  ;; asap articles often set year to 0, which messes up key
  ;; generation. fix that.
  (let ((year (bibtex-autokey-get-field "year")))
    (when (string= "0" year)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "year" t))))
      (bibtex-kill-field)
      (bibtex-make-field "year")
      (backward-char)
      (insert (or new-year (read-string "Enter year: ")))))

  "Check for empty pages, and put eid in its place if it exists."
  (let ((pages (bibtex-autokey-get-field "pages"))
 (eid (bibtex-autokey-get-field "eid")))
    (when (and (not (string= "" eid))
        (or (string= "" pages)))
      (bibtex-set-field "pages" eid)))

  "Remove http://dx.doi.org/ in the doi field."
  (let ((doi (bibtex-autokey-get-field "doi")))
    (when (string-match "^http://dx.doi.org/" doi)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "doi" t))))
      (bibtex-kill-field)
      (bibtex-make-field "doi")
      (backward-char)
      (insert (replace-regexp-in-string "^http://dx.doi.org/" "" doi))))

  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (entry-fields)
         (other-fields)
         (type (cdr (assoc "=type=" entry)))
         (key (cdr (assoc "=key=" entry)))
  (field-order (cdr (assoc (if type (downcase type))
                            bibtex-sort-order))))

    ;; these are the fields we want to order that are in this entry
    (setq entry-fields (mapcar (lambda (x) (car x)) entry))
    ;; we do not want to reenter these fields
    (setq entry-fields (remove "=key=" entry-fields))
    (setq entry-fields (remove "=type=" entry-fields))

    ;;these are the other fields in the entry, and we sort them alphabetically.
    (setq other-fields
   (sort (-remove (lambda(x) (member x field-order)) entry-fields)
  'string<))

    (save-restriction
      (bibtex-kill-entry)
      (insert
       (concat "@" type "{" key ",\n"
        (mapconcat
         (lambda (field)
    (when (member field entry-fields)
      (format "%s = %s,"
       field
       (cdr (assoc field entry)))))
         field-order "\n")
        ;; now add the other fields
        (mapconcat
         (lambda (field)
    (cl-loop for (f . v) in entry concat
      (when (string= f field)
        (format "%s = %s,\n" f v))))
         (-uniq other-fields) "\n")
        "\n}\n\n"))
      (bibtex-find-entry key)
      (bibtex-fill-entry)
      (bibtex-clean-entry)))

 "Replace the key in the entry.
Prompts for replacement if the new key duplicates one already in
the file, unless ALLOW-DUPLICATE-KEYS is non-nil."
  (let ((key (funcall org-ref-clean-bibtex-key-function
        (bibtex-generate-autokey))))
    ;; remove any \\ in the key
    (setq key (replace-regexp-in-string "\\\\" "" key))
    ;; first we delete the existing key
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
 (delete-region (match-beginning bibtex-key-in-head)
         (match-end bibtex-key-in-head)))
    ;; check if the key is in the buffer
    (when (and (not allow-duplicate-keys)
               (save-excursion
                 (bibtex-search-entry key)))
      (save-excursion
 (bibtex-search-entry key)
 (bibtex-copy-entry-as-kill)
 (switch-to-buffer-other-window "*duplicate entry*")
 (bibtex-yank))
      (setq key (bibtex-read-key "Duplicate Key found, edit: " key)))
    (insert key)
    (kill-new key))
  )

;;;###autoload
(defun bibtex-set-field (field value &optional nodelim)
  "Set FIELD to VALUE in bibtex file.  create field if it does not exist.
Optional argument NODELIM see `bibtex-make-field'."
  (interactive "sfield: \nsvalue: ")
  (bibtex-beginning-of-entry)
  (let ((found))
    (if (setq found (bibtex-search-forward-field field t))
        ;; we found a field
        (progn
          (goto-char (car (cdr found)))
          (when value
            (bibtex-kill-field)
            (bibtex-make-field field nil nil nodelim)
            (backward-char)
            (insert value)))
      ;; make a new field
      (bibtex-beginning-of-entry)
      (forward-line) (beginning-of-line)
      (bibtex-next-field nil)
      (forward-char)
      (bibtex-make-field field nil nil nodelim)
      (backward-char)
      (insert value))))

;;;###autoload
(defun bibtex-set-keywords (keywords &optional arg)
  "Add KEYWORDS to a bibtex entry.
If KEYWORDS is a list, it is converted to a comma-separated
string.  The KEYWORDS are added to the beginning of the
field.  Otherwise KEYWORDS should be a string of comma-separate
keywords.  Optional argument ARG prefix arg to replace keywords."
  (interactive
   (list
    (completing-read "Keyword: " (bibtex-keywords))
    current-prefix-arg))
  (bibtex-set-field
   "keywords"
   (if arg
       ;; replace with arg
       (if (listp keywords)
           (mapconcat 'identity keywords ", ")
         keywords)
     ;; else concatentate
     (concat
      (if (listp keywords)
          (mapconcat 'identity keywords ", ")
        keywords)
      (when (not (string= "" (bibtex-autokey-get-field "keywords")))
        (concat ", "  (bibtex-autokey-get-field "keywords"))))))
  (when (buffer-file-name)
    (save-buffer)))


;;;###autoload
(defun bibtex-keywords ()
  "Get keywords defined in current bibtex file.
These are in the keywords field, and are comma or semicolon separated."
  (save-excursion
    (goto-char (point-min))
    (let (keywords kstring)
      (while (re-search-forward "^\\s-*keywords.*{\\([^}]+\\)}" nil t)
        ;; TWS - remove newlines/multiple spaces:
        (setq kstring (replace-regexp-in-string
                       "[ \t\n]+" " "
                       (match-string 1)))
        (mapc
         (lambda (v)
           (add-to-list 'keywords v t))
         (split-string kstring "\\(,\\|;\\)[ \n]*\\|{\\|}" t)))
      keywords)))

;;* The org-mode link
;; this just makes a clickable link that opens the entry.
;; example: arxiv:cond-mat/0410285
(org-ref-link-set-parameters "arxiv"
  :follow (lambda (link-string)
            (browse-url (format "http://arxiv.org/abs/%s" link-string)))
  :export (lambda (keyword desc format)
            (cond
             ((eq format 'html)
              (format  "<a href=\"http://arxiv.org/abs/%s\">arxiv:%s</a>"
                       keyword  (or desc keyword)))
             ((eq format 'latex)
              ;; write out the latex command
              (format "\\url{http://arxiv.org/abs/%s}{%s}" keyword (or desc keyword))))))

;;* Getting a bibtex entry for an arXiv article using remote service:
;; For an arxiv article, there is a link to a NASA ADS page like this:
;; http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:1503.01742
;; On that page, there is a link to a bibtex entry:
;; http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=2015arXiv150301742H&data_type=BIBTEX&db_key=PRE&nocookieset=1
;;
;; It looks like you need to get a Bibliographic code from the arxiv number to
;; then get the bibtex entry.

(defun arxiv-get-bibliographic-code (arxiv-number)
  "Get Bibliographic code for ARXIV-NUMBER."
  (with-current-buffer
      (url-retrieve-synchronously
       (concat
        "http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:"
        arxiv-number))
    (search-forward-regexp "name=\\\"bibcode\\\" value=\\\"\\(.*\\)\\\"")
    (match-string 1)))


(defun arxiv-get-bibtex-entry (arxiv-bibliographic-code)
  "Get bibtex entry for ARXIV-BIBLIOGRAPHIC-CODE."
  (with-current-buffer
      (url-retrieve-synchronously
       (format
        "http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=%s&data_type=BIBTEX&db_key=PRE&nocookieset=1"
        arxiv-bibliographic-code))
    (goto-char url-http-end-of-headers)
    (if (search-forward  "Retrieved 1 abstracts" (point-max) t)
        (progn
          (forward-line)
          (buffer-substring (point) (point-max)))
      (error "Did not get one entry: %s" (buffer-substring (point) (point-max))))))

;;* Getting a bibtex entry for an arXiv article using arXiv API:
;; Retrieves the meta data of an article view arXiv's http API,
;; extracts the necessary information, and formats a new BibTeX entry.

(defvar arxiv-entry-format-string "@article{%s,
  journal = {CoRR},
  title = {%s},
  author = {%s},
  archivePrefix = {arXiv},
  year = {%s},
  eprint = {%s},
  primaryClass = {%s},
  abstract = {%s},
  url = {%s},
}"
	"Template for BibTeX entries of arXiv articles.")


(defun arxiv-get-bibtex-entry-via-arxiv-api (arxiv-number)
  "Retrieve meta data for ARXIV-NUMBER.
Returns a formatted BibTeX entry."
  (with-current-buffer
      (url-retrieve-synchronously (format "http://export.arxiv.org/api/query?id_list=%s" arxiv-number) t)
    (let* ((parse-tree (libxml-parse-xml-region
                        (progn (goto-char 0)
                               (search-forward "<?xml ")
                               (match-beginning 0))
                        (point-max)))
           (entry (assq 'entry parse-tree))
           (authors (--map (nth 2 (nth 2 it))
                           (--filter (and (listp it) (eq (car it) 'author)) entry)))
           (year (format-time-string "%Y" (date-to-time (nth 2 (assq 'published entry)))))
           (title (nth 2 (assq 'title entry)))
           (names (arxiv-bibtexify-authors authors))
           (category (cdar (nth 1 (assq 'primary_category entry))))
           (abstract (s-trim (nth 2 (assq 'summary entry))))
           (url (nth 2 (assq 'id entry)))
           (temp-bibtex (format arxiv-entry-format-string "" title names year arxiv-number category abstract url))
           (key (with-temp-buffer
                  (insert temp-bibtex)
		  (bibtex-mode)
		  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                  (bibtex-generate-autokey))))
      (format arxiv-entry-format-string key title names year arxiv-number category abstract url))))


(defun arxiv-bibtexify-authors (authors)
  "Return names in 'SURNAME, FIRST NAME' format from AUTHORS list."
  (s-join " and "
          (--map (concat (-last-item it) ", " (s-join " " (-remove-last 'stringp it)))
                 (--map (s-split " +" it) authors))))


;;;###autoload
(defun arxiv-add-bibtex-entry (arxiv-number bibfile)
  "Add bibtex entry for ARXIV-NUMBER to BIBFILE."
  (interactive
   (list (read-string "arxiv: ")
         ;;  now get the bibfile to add it to
         (completing-read
          "Bibfile: "
          (append (f-entries "." (lambda (f) (f-ext? f "bib")))
                  org-ref-default-bibliography))))
  (save-window-excursion
    (find-file bibfile)
    (goto-char (point-max))
    (when (not (looking-at "^")) (insert "\n"))
    (insert (arxiv-get-bibtex-entry-via-arxiv-api arxiv-number))
    (clean-and-sort-bibtex-entry)
    (goto-char (point-max))
    (when (not (looking-at "^")) (insert "\n"))
    (save-buffer)))


;;;###autoload
(defun arxiv-get-pdf (arxiv-number pdf)
  "Retrieve a pdf for ARXIV-NUMBER and save it to PDF."
  (interactive "sarxiv: \nsPDF: ")
  (let ((pdf-url (with-current-buffer
                     (url-retrieve-synchronously
                      (concat
                       "http://arxiv.org/abs/" arxiv-number))
                   ;; <meta name="citation_pdf_url" content="http://arxiv.org/pdf/0801.1144" />
                   (goto-char (point-min))
                   (search-forward-regexp
                    "name=\\\"citation_pdf_url\\\" content=\\\"\\(.*\\)\\\"")
                   (match-string 1))))
   (start-process "" nil "wget" "--user-agent='NOT WGET'" pdf-url (concat "-O" pdf))
    ))

;;;###autoload
(defun arxiv-get-pdf-add-bibtex-entry (arxiv-number bibfile pdfdir)
  "Add bibtex entry for ARXIV-NUMBER to BIBFILE.
Remove troublesome chars from the bibtex key, retrieve a pdf
for ARXIV-NUMBER and save it to PDFDIR with the same name of the
key."
  (interactive
   (list (read-string "arxiv: ")
         ;;  now get the bibfile to add it to
         (completing-read
          "Bibfile: "
          (append (f-entries "." (lambda (f) (f-ext? f "bib")))
                  org-ref-default-bibliography))
         (read-directory-name
          "PDF directory: "
          org-ref-pdf-directory)))
  (string-match "[0-9]+.[0-9]+" arxiv-number)
  (setq arxiv-number (match-string 0 arxiv-number))
  (arxiv-add-bibtex-entry arxiv-number bibfile)

  (save-window-excursion
    (let ((key ""))
      (find-file bibfile)
      (goto-char (point-max))
      (bibtex-beginning-of-entry)
      (re-search-forward bibtex-entry-maybe-empty-head)
      (if (match-beginning bibtex-key-in-head)
          (progn
            (setq key (delete-and-extract-region
                       (match-beginning bibtex-key-in-head)
                       (match-end bibtex-key-in-head)))
            ;; remove potentially troublesome characters from key
            ;; as it will be used as  a filename
            (setq key (replace-regexp-in-string   "\"\\|\\*\\|/\\|:\\|<\\|>\\|\\?\\|\\\\\\||\\|\\+\\|,\\|\\.\\|;\\|=\\|\\[\\|]\\|!\\|@"
                                                  "" key))
            ;; check if the key is in the buffer
            (when (save-excursion
                    (bibtex-search-entry key))
              (save-excursion
                (bibtex-search-entry key)
                (bibtex-copy-entry-as-kill)
                (switch-to-buffer-other-window "*duplicate entry*")
                (bibtex-yank))
              (setq key (bibtex-read-key "Duplicate Key found, edit: " key))))
        (setq key (bibtex-read-key "Key not found, insert: ")))
      (insert key)
      (save-some-buffers bibfile)
      (arxiv-get-pdf arxiv-number (concat pdfdir key ".pdf")))))

(provide 'org-ref-arxiv)
;;; org-ref-arxiv.el ends here
