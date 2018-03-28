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

(defgroup define-word nil
  "Define word at point using an online dictionary."
  :group 'convenience
  :prefix "define-word-")

(defvar define-word-limit 10
  "Maximum amount of results to display.")

(defcustom define-word-unpluralize t
  "When non-nil, change the word to singular when appropriate.
The rule is that all definitions must contain \"Plural of\"."
  :type 'boolean)

(defcustom define-word-services
  '((wordnik "http://wordnik.com/words/%s" define-word--parse-wordnik nil))
  "Services for define-word, A list of lists of the
  format (symbol url function-for-parsing [function-for-display])"
  :type '(alist :key-type (symbol :tag "Name of service")
          :value-type (group
                       (string :tag "Url (%s denotes search word)")
                       (function :tag "Parsing function")
                       (choice (const nil) (function :tag "Display function")))))

(defcustom define-word-default-service 'wordnik
  "The default service for define-word commands. Must be one of
  `define-word-services'"
  :type 'symbol)

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
                            "Service: " (mapcar #'car define-word-services)))
                        define-word-default-service)))
         (servicedata (assoc service define-word-services))
         (parser (nth 2 servicedata))
         (displayfn (or (nth 3 servicedata) #'message))
         (link (format (nth 1 servicedata) (downcase word)))
         (results
          (with-current-buffer (url-retrieve-synchronously link t t)
            (funcall parser))))
    (if results
        (funcall displayfn results)
      (message "0 definitions found")
      nil)))

;;;###autoload
(defun define-word-at-point (arg &optional service)
  "Use `define-word' to define word at point.
When the region is active, define the marked phrase.
Prefix ARG lets you choose service.

In a non-interactive call SERVICE can be passed."
  (interactive "P")
  (if (use-region-p)
      (define-word
          (buffer-substring-no-properties
           (region-beginning)
           (region-end))
          service arg)
    (define-word (substring-no-properties
                  (thing-at-point 'word))
        service arg)))

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
          (push (concat (propertize part 'face 'font-lock-keyword-face)
                        (buffer-substring-no-properties beg (match-beginning 0)))
                results)))
      (setq results (nreverse results))
      (cond ((= 0 (length results))
             (message "0 definitions found"))
            ((and define-word-unpluralize
                  (cl-every (lambda (x) (string-match "[Pp]\\(?:lural\\|l\\.\\).*of \\(.*\\)\\." x))
                            results))
             (define-word (match-string 1 (car (last results))) 'wordnik))
            (t
             (when (> (length results) define-word-limit)
               (setq results (cl-subseq results 0 define-word-limit)))
             (mapconcat #'identity results "\n"))))))

(provide 'define-word)

;;; define-word.el ends here
