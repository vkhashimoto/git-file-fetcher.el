;;; git-file-fetcher.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  vkhashimoto

;; Author: vkhashimoto <me@vkhashimoto.dev>
;; Maintainer: vkhashimoto <me@vkhashimoto.dev>
;; URL: 
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.3"))
;; Keywords: 

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; 
;;

;;; Code:

;; Happy coding! ;)

;;; Git providers

(setq git-providers (make-hash-table :test 'equal))

(defun ff/--parse-github (repo)
  (let* ((host "github.com")
	 (url "https://github.com/")
	 (repo (nth 1(split-string repo ":"))))
	 (list host url repo)))

(defun ff/add-git-provider (name parser)
  (puthash name parser git-providers))

(ff/add-git-provider "github" 'ff/--parse-github)

(defun ff/--get-provider-name (repo)
  (let ((provider-name (nth 0 (split-string repo ":"))))
    provider-name))

(defun ff/--get-provider-info (repo)
  (funcall (gethash (ff/--get-provider-name repo) git-providers) repo))

(defun ff/--get-directory (repo)
  (let ((info (ff/--get-provider-info repo)))
	(format "%s/%s/%s" "./tmp" (nth 0 info) (nth 2 info))))


;;; Git operations

(defun ff/--clone (repo)
  (let ((info (ff/--get-provider-info repo)))
    (vc-clone (format "%s/%s" (nth 1 info) (nth 2 info)) 'Git (format "%s/%s/%s" "./tmp" (nth 0 info) (nth 2 info)))
))

(defun ff/--checkout (repo rev)
  (let* ((info (ff/--get-provider-info repo))
	 (current-dir default-directory)
	(repo-dir (format "%s%s/%s/%s" current-dir "tmp" (nth 0 info) (nth 2 info))))
    (if (not (file-directory-p repo-dir))
	(error (format "No such directory %s. Probably there is some error with the current-buffer directory." repo-dir)))
    (let ((default-directory repo-dir))
	(vc-git-command nil 0 nil "checkout" rev))
    ))


(defun ff/--get-latest-commit (repo)
  (let* ((info (ff/--get-provider-info repo))
	 (current-dir default-directory)
	 (current-revision)
	(repo-dir (format "%s%s/%s/%s" current-dir "tmp" (nth 0 info) (nth 2 info))))
    (if (not (file-directory-p repo-dir))
	(error (format "No such directory %s. Probably there is some error with the current-buffer directory." repo-dir)))

    (let ((default-directory repo-dir))
	(setq current-revision (vc-git-working-revision "")))
    current-revision))

;;; Main operations

(defun ff/get-content (&key repo &key file &key lines &key rev)
  (when (not (file-directory-p (ff/--get-directory repo)))
    (ff/--clone repo))
  (if rev
      (ff/--checkout repo rev))
  (let* ((repo-info (ff/--get-provider-info repo))
	 (directory (ff/--get-directory repo))
	 (start-line (string-to-number (nth 0 (string-split lines ","))))
	 (end-line (string-to-number (nth 1 (string-split lines ","))))
	 (content (with-temp-buffer
		    (insert-file-contents (format "%s/%s" directory file))
		    (buffer-string)))
	 (lines (split-string content "\n"))
	 (lines (ntake end-line lines))
	 (lines (reverse (ntake (- end-line (- start-line 1)) (reverse lines)))))
    (string-join lines "\n")
))

;; TODO: Maybe accept params in any order
(defun ff/--get-regex-parse-line (line)
  (if (string-match-p "\\:rev" line)
      ":repo \"\\(.*\\)\" :file \"\\(.*\\)\" :lines \"\\(.*\\)\" :rev \"\\(.*\\)\"" 
    ":repo \"\\(.*\\)\" :file \"\\(.*\\)\" :lines \"\\(.*\\)\""))

(defun ff/--parse-line (line)
  (let* ((regex (ff/--get-regex-parse-line line))
	(result (when (string-match regex line)
		  (list (match-string 1 line) (match-string 2 line) (match-string 3 line) (match-string 4 line)))))
    (seq-filter (lambda (item) item )result)))

(defun ff/--get-line-config ()
  (let* ((ff-line (thing-at-point 'line t))
	 (ff-line (string-trim ff-line))
	 (ff-line (if (not (string-match-p "# ff:" ff-line))
		     (user-error "Invalid git-fetcher line")
		   (replace-regexp-in-string "^# ff: " "" ff-line)))
	(parsed-line (ff/--parse-line ff-line)))
    parsed-line
    ))

(defun ff/get-content-from-line ()
  (interactive)
  (let* ((ff-line (thing-at-point 'line t))
	(ff-line (string-trim ff-line))
	(ff-line (if (not (string-match-p "# ff:" ff-line))
		     (user-error "Invalid git-fetcher line")
		   (replace-regexp-in-string "^# ff: " "" ff-line)))
	(parsed-line (ff/--parse-line ff-line)))
    (ff/get-content :repo (nth 0 parsed-line) :file (nth 1 parsed-line) :lines (nth 2 parsed-line) :rev (nth 3 parsed-line))
))

(defun ff/replace-src-block-from-line ()
  (interactive)
  (let* ((content (ff/get-content-from-line))
	(line-config (ff/--get-line-config))
	(rev (nth 3 line-config))
	(current-revision (ff/--get-latest-commit (nth 0 line-config)))
	)
    (save-excursion
      (if (not rev)
	  (progn
	    (end-of-line)
	    (insert (format " :rev \"%s\"" current-revision)))
	(progn
	  (if (not (s-starts-with? rev current-revision))
	      (progn 
		(beginning-of-line)
		(search-forward ":rev")
		(delete-region (point) (line-end-position))
		(insert (format " \"%s\"" current-revision))
	  ))))
      (search-forward "#+begin_src")
      (end-of-line)
      (insert  "\n")
      (insert content)
      )
    ))

(provide 'git-file-fetcher)
;;; git-file-fetcher.el ends here
