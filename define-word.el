;;; define-word.el --- display the definition of word at point. -*- lexical-binding: t -*-

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/define-word
;; Version: 0.1.0
;; Keywords: dictionary, convenience

;;; Commentary:
;;
;; This package will send an anonymous request to http://wordnik.com/
;; to get the definition of word or phrase at point, parse the resulting HTML
;; page, and display it with `message'.
;;
;; The HTML page is retrieved asynchronously, using `url-retrieve'.

(require 'url-parse)
(require 'url-http)

;;; Code:

(defconst define-word-limit 10
  "Maximum amount of results to display.")

;;;###autoload
(defun define-word (word)
  "Define WORD using the Wordnik website."
  (interactive (list (read-string "Word: ")))
  (let ((link (concat "http://wordnik.com/words/" (downcase word))))
    (url-retrieve
     link
     (lambda (status)
       (let ((err (plist-get status :error)))
         (if err (error
                  "\"%s\" %s" link
                  (downcase (nth 2 (assq (nth 2 err) url-http-codes)))))
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
           (when (> (length results) define-word-limit)
             (setq results (cl-subseq results 0 define-word-limit)))
           (if results
               (message (mapconcat #'identity results "\n"))
             (message "0 definitions found")))))
     nil
     t t)))

;;;###autoload
(defun define-word-at-point ()
  "Use `define-word' to define word at point.
When the region is active, define the marked phrase."
  (interactive)
  (if (region-active-p)
      (define-word
          (buffer-substring-no-properties
           (region-beginning)
           (region-end)))
    (define-word (thing-at-point 'word))))

(provide 'define-word)

;;; define-word.el ends here
