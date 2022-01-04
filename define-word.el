;;; define-word.el --- display the definition of word at point. -*- lexical-binding: t -*-

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/define-word
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: dictionary, convenience

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package will send an anonymous request to https://wordnik.com/
;; to get the definition of word or phrase at point, parse the resulting HTML
;; page, and display it with `message'.
;;
;; Extra services can be added by customizing `define-word-services'
;; where an url, a parsing function, and an (optional) function other
;; than `message' to display the results can be defined.
;;
;; The HTML page is retrieved asynchronously, using `url-retrieve-link'.
;;
;;; Code:

(require 'url-parse)
(require 'url-http)
(require 'nxml-mode)

(defgroup define-word nil
  "Define word at point using an online dictionary."
  :group 'convenience
  :prefix "define-word-")

(defvar define-word-limit 10
  "Maximum amount of results to display.")

(defcustom define-word-displayfn-alist nil
  "Alist for display functions per service.
By default, `message' is used."
  :type '(alist
          :key-type (symbol :tag "Name of service")
          :value-type (function :tag "Display function")))

(defun define-word-displayfn (service)
  "Return the display function for SERVICE."
  (or (cdr (assoc service define-word-displayfn-alist))
      #'message))

(defcustom define-word-services
  '((wordnik "http://wordnik.com/words/%s" define-word--parse-wordnik)
    (openthesaurus "https://www.openthesaurus.de/synonyme/%s" define-word--parse-openthesaurus)
    (webster "http://webstersdictionary1828.com/Dictionary/%s" define-word--parse-webster)
    (offline-wikitionary define-word--get-offline-wikitionary nil))
  "Services for define-word, A list of lists of the
  format (symbol url function-for-parsing).
Instead of an url string, url can be a custom function for retrieving results."
  :type '(alist
          :key-type (symbol :tag "Name of service")
          :value-type (group
                       (string :tag "Url (%s denotes search word)")
                       (function :tag "Parsing function"))))

(defcustom define-word-default-service 'wordnik
  "The default service for define-word commands. Must be one of
  `define-word-services'"
  :type '(choice
          (const wordnik)
          (const openthesaurus)
          (const webster)
          (const offline-wikitionary)
          symbol))

(defvar define-word-offline-dict-directory nil
  "Path to the directory which contains \"en-en-withforms-enwiktionary.txt\".")

(defun define-word--get-offline-wikitionary (word)
  (unless define-word-offline-dict-directory
    (let ((url "https://en.wiktionary.org/wiki/User:Matthias_Buchmeier/download"))
      (user-error "Please download the ding (text-format) zip from %s and configure `%S'." url
                  'define-word-offline-dict-directory)))
  (let* ((regex (concat "^" word " "))
         (default-directory define-word-offline-dict-directory)
         (res (shell-command-to-string
               (concat "rg --no-filename --color never '" regex "'"))))
    (unless (= 0 (length res))
      res)))

(defun define-word--to-string (word service)
  "Get definition of WORD from SERVICE."
  (let* ((servicedata (assoc service define-word-services))
         (retriever (nth 1 servicedata))
         (parser (nth 2 servicedata))
         (url-user-agent
          (if (eq (nth 0 servicedata) 'wordnik)
              "Mozilla/5.0 (Macintosh; Intel Mac OS X 11_5_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/93.0.4577.63 Safari/537.36"
            url-user-agent)))
    (if (functionp retriever)
        (funcall retriever word)
      ;; adapted `url-insert-file-contents'
      (let* ((url (format retriever (downcase word)))
             (buffer (url-retrieve-synchronously url t t)))
        (with-temp-buffer
          (url-insert-buffer-contents buffer url)
          (funcall parser))))))

(defun define-word--expand (regex definition service)
  (let ((case-fold-search nil))
    (when (string-match regex definition)
      (concat
       definition
       "\n" (match-string 1 definition) ":\n"
       (mapconcat (lambda (s) (concat "  " s))
                  (split-string
                   (define-word--to-string (match-string 1 definition) service)
                   "\n")
                  "\n")))))

;;;###autoload
(defun define-word (word service &optional choose-service)
  "Define WORD using various services.

By default uses `define-word-default-service', but a prefix arg
lets the user choose service."
  (interactive "MWord: \ni\nP")
  (let* ((service (or service
                      (if choose-service
                          (intern
                           (completing-read
                            "Service: " define-word-services))
                        define-word-default-service)))
         (results (define-word--to-string word service)))

    (funcall
     (define-word-displayfn service)
     (cond ((not results)
            "0 definitions found")
           ((define-word--expand "Plural form of \\(.*\\)\\.$" results service))
           ((define-word--expand "Past participle of \\(.*\\)\\.$" results service))
           ((define-word--expand "Present participle of \\(.*\\)\\.$" results service))
           (t
            results)))))

(declare-function pdf-view-active-region-text "ext:pdf-view")

;;;###autoload
(defun define-word-at-point (arg &optional service)
  "Use `define-word' to define word at point.
When the region is active, define the marked phrase.
Prefix ARG lets you choose service.

In a non-interactive call SERVICE can be passed."
  (interactive "P")
  (let ((word
         (cond
          ((eq major-mode 'pdf-view-mode)
           (car (pdf-view-active-region-text)))
          ((use-region-p)
           (buffer-substring-no-properties
            (region-beginning)
            (region-end)))
          (t
           (substring-no-properties
            (thing-at-point 'word))))))
    (define-word word service arg)))

(defface define-word-face-1
  '((t :inherit font-lock-keyword-face))
  "Face for the part of speech of the definition.")

(defface define-word-face-2
  '((t :inherit default))
  "Face for the body of the definition")

(defun define-word--join-results (results)
  (mapconcat
   #'identity
   (if (> (length results) define-word-limit)
       (cl-subseq results 0 define-word-limit)
     results)
   "\n"))

(defun define-word--regexp-to-face (regexp face)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (let ((match (match-string 1)))
      (replace-match
       (propertize match 'face face)))))

(defconst define-word--tag-faces
  '(("<\\(?:em\\|i\\)>\\(.*?\\)</\\(?:em\\|i\\)>" italic)
    ("<xref>\\(.*?\\)</xref>" link)
    ("<strong>\\(.*?\\)</strong>" bold)
    ("<internalXref.*?>\\(.*?\\)</internalXref>" default)))

(defun define-word--convert-html-tag-to-face (str)
  "Replace semantical HTML markup in STR with the relevant faces."
  (with-temp-buffer
    (insert str)
    (cl-loop for (regexp face) in define-word--tag-faces do
         (define-word--regexp-to-face regexp face))
    (buffer-string)))

(defun define-word--parse-wordnik ()
  "Parse output from wordnik site and return formatted list"
  (save-match-data
    (let (results beg part)
      (while (re-search-forward "<li><abbr[^>]*>\\([^<]*\\)</abbr>" nil t)
        (setq part (match-string 1))
        (unless (= 0 (length part))
          (setq part (concat part " ")))
        (skip-chars-forward " ")
        (setq beg (point))
        (when (re-search-forward "</li>")
          (push (concat (propertize part 'face 'define-word-face-1)
                        (propertize
                         (buffer-substring-no-properties beg (match-beginning 0))
                         'face 'define-word-face-2))
                results)))
      (when (setq results (nreverse results))
        (define-word--convert-html-tag-to-face (define-word--join-results results))))))

(defun define-word--parse-webster ()
  "Parse definition from webstersdictionary1828.com."
  (save-match-data
    (goto-char (point-min))
    (let (results def-type)
      (while (re-search-forward "<p><strong>\\(?:[[:digit:]]\\.\\)?.*</strong>\\(.*?\\)</p>" nil t)
        (save-match-data
          (save-excursion
            (re-search-backward "<p><strong>[A-Z'.]*</strong>, <em>\\(.*?\\)</em>")
            (let ((match (match-string 1)))
              (setq def-type
                    (cond
                      ((equal match "adjective") "adj.")
                      ((equal match "noun") "n.")
                      ((equal match "verb intransitive") "v.")
                      ((equal match "verb transitive") "vt.")
                      (t ""))))))
        (push
         (concat
          (propertize def-type 'face 'bold)
          (define-word--convert-html-tag-to-face (match-string 1)))
         results))
      (when (setq results (nreverse results))
        (define-word--join-results results)))))

(defun define-word--parse-openthesaurus ()
  "Parse output from openthesaurus site and return formatted list"
  (save-match-data
    (let (results part beg)
      (goto-char (point-min))
      (nxml-mode)
      (while (re-search-forward "<sup>" nil t)
        (goto-char (match-beginning 0))
        (setq beg (point))
        (nxml-forward-element)
        (delete-region beg (point)))
      (goto-char (point-min))
      (while (re-search-forward
              "<span class='wiktionaryItem'> [0-9]+.</span>\\([^<]+\\)<" nil t)
        (setq part (match-string 1))
        (backward-char)
        (push (string-trim part) results))
      (when (setq results (nreverse results))
        (define-word--join-results results)))))

(provide 'define-word)

;;; define-word.el ends here
