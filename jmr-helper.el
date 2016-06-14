;;; jmr-helper.el --- purpose

;; Copyright (C) 2016 Rock Neurotiko
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Rock Neurotiko <miguelglafuente AT gmail DOT com>
;; URL: http://github.com/usrname/jmr-helper
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;;; Code:

(require 'dash)                         ;https://github.com/magnars/dash.el

(defun jmr--strip-text-properties (txt)
  "Remove all the text properties from `TXT'."
  (set-text-properties 0 (length txt) nil txt)
  txt)

;; From JDEE <3

(defun jmr--contains (list elem)
  (let ((cont nil))
    (dolist (x (append list nil))       ;Convert to list
      (when (equal x elem)
        (setq cont t)))
    cont))

(defun jmr--import-list ()
  "Build the list of java package declared in the current buffer.
It mostly scans the buffer for 'import' statements, and return the
resulting list.  It impliciyly adds the java.lang.* and current.package.* packages."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (lst first second cpackage)
      (ignore-errors (re-search-forward "package \\(\\([a-zA-Z0-9]+[.]\\)*[a-zA-Z0-9]+\\)"))
      (setq cpackage (jmr--strip-text-properties (or (ignore-errors (concat (match-string 1) ".")) "")))

      (while (not (null
                   (re-search-forward "import[ \t\n\r]+\\(\\([a-zA-Z0-9]+[.]\\)+\\)\\([*]\\|[a-zA-Z0-9]+\\)" nil t) ))
        (setq first (jmr--strip-text-properties (match-string 1)))
        (setq second (jmr--strip-text-properties (match-string 3)))
        (if (string= "*" second)        ;; Maybe load all with jar tf?
            (setq lst (append lst
                              (list (list first second))))
          (setq lst (append (list (list first second))
                            lst))))
      (when (not (member (concat cpackage "*") lst))
        (setq lst (append lst (list (list cpackage "*")))))
      (when (not (member "java.lang.*" lst))
        (setq lst (append lst (list (list "java.lang." "*")))))
      lst)))

(defun jmr--filter-fqn (importlist)
  "Filter all the fully-qualified classnames in the `IMPORTLIST'.
It uses the knowledge that those classnames are at the beginning of the list,
so that it can stops at the first package import (with a star `*' at
the end of the declaration)."
  (if (not (null importlist))
      (if (string= "*" (car (cdr (car importlist))))
          importlist
        (jmr--filter-fqn (cdr importlist)))))

;;; JMR code

(defun jmr--is-comment-line ()
  "Check if the line at current point it's not a comment."
  (save-excursion
    (goto-char (line-beginning-position))
    (and (re-search-forward "//.*$" (line-end-position) t nil) t)))

(defun jmr--in-block-comment ()
  "Check if the current point it's in a block comment."
  (save-excursion
    (let ((startpoint (point))
          (startbc (re-search-backward "/\\*" nil t nil))
          (endbc (or (re-search-forward "\\*/" nil t nil) 0)))
      (and (not (null startbc))  (<= startbc endbc) (< startbc startpoint) (> endbc startpoint)))))

(defun jmr--valid-java-declaration-at (point varname)
  "Verify that a `POINT' start a valid java declaration for the `VARNAME' variable."
  (save-excursion
    (goto-char point)
    (if (looking-at (concat "\\([A-Za-z_.][\]\[<>A-Za-z_.0-9]*\\)[ \t\n\r]+" varname "[ \t\n\r]*[;=,)]")) ;With , and ) to handle methods args!
        (let ((match (match-string 1))
              (isc (or (jmr--is-comment-line) (jmr--in-block-comment))))
          (if (or isc (string= match "return")) nil match))
      nil)))

(defun jmr--count-string (str subs)
  (--count (equal str (string it)) (append subs nil)))

(defun jmr--pointer-in-string ()
  (let ((leftdq (jmr--count-string "\"" (buffer-substring (line-beginning-position) (point))))
        (rightdq (jmr--count-string "\"" (buffer-substring (point) (line-end-position)))))
    (and (= (% leftdq 2) 1)
         (= (% rightdq 2) 1))))

;;; All the substrings maybe look until { or ; ??
(defun jmr--pointer-inside-call ()
  (let* ((lefts (buffer-substring (line-beginning-position) (point)))
         (rights (buffer-substring (point) (line-end-position)))
         (leftpopen (jmr--count-string "(" lefts))
         (leftpclose (jmr--count-string ")" lefts))
         (rightpopen (jmr--count-string "(" rights))
         (rightpclose (jmr--count-string ")" rights)))
    (and (not (equal leftpopen leftpclose)) (not (equal rightpopen rightpclose)))))

(defun jmr--pointer-function-inside-counter ()
  (let* ((lefts (buffer-substring (line-beginning-position) (point)))
         (rights (buffer-substring (point) (line-end-position)))
         (leftpopen (jmr--count-string "(" lefts))
         (leftpclose (jmr--count-string ")" lefts)))
    (- leftpopen leftpclose)))

(defun jmr--expr-walker (&optional first left)
  (save-excursion
    (let ((instr (jmr--pointer-in-string))
          (callcount (jmr--pointer-function-inside-counter))
          escape
          done
          (elem (string (following-char)))
          (returnp (point)))
      (while (and (not done) (>= callcount 0))
        (setq returnp (point))
        (cond
         (instr
          (cond
           ((equal elem "\"")
            (setq instr (if escape t nil))
            (setq escape nil))
           ((equal elem "\\")
            (setq escape t))))
         ((and first (= callcount 0) (equal elem "."))
          (setq done t))
         ((and first (= callcount 0) (equal elem " "))
          (setq done t))
         ((and first (equal elem "="))
          (setq done t))
         ((equal elem "\"")
          (setq instr (not instr)))
         ((equal elem "(")
          (setq callcount (+ callcount 1)))
         ((equal elem ")")
          (setq callcount (- callcount 1)))
         ((equal elem ";")
          (setq done t))
         ((equal elem "{")
          (setq done t)))

        ;; (if left (left-char 1) (right-char 1))
        (right-char 1)
        (setq elem (string (following-char))))
      ;; (when left
      ;;   (right-char 1)
      ;;   (search-forward-regexp "[^\s\t\n;{]")
      ;;   (left-char 1)
      ;;   (setq returnp (point)))
      returnp)))

;; Error when string inside call :(
(defun jmr--get-current-expression ()
  (let ((actual (string (following-char))) origp firstp lastp)
    (unless (equal "" actual)
      (setq origp (point))
      (re-search-backward "[^\]\[><A-Za-z_.0-9()]" nil t nil)
      (right-char 1)
      (setq firstp (point))
      (goto-char origp)
      (setq lastp (jmr--expr-walker t))
      (jmr--strip-text-properties (buffer-substring firstp lastp)))))

(defun jmr--get-current-full-expression ()
  (let ((actual (string (following-char))) origp firstp lastp)
    (unless (equal "" actual)
      (setq origp (point))
      (re-search-backward "[^\]\[><A-Za-z_.0-9()]" nil t nil)
      (right-char 1)
      (setq firstp (point))
      (goto-char origp)
      (setq lastp (jmr--expr-walker))
      (jmr--strip-text-properties (buffer-substring firstp lastp)))))

(defun jmr--split-expression-in-call (expr)
  (let (instr escape (callcount 0) (actual "") (elem (substring expr 0 1)) (rest (substring expr 1)) (result '()))
    (while (or (not (equal elem "")) (not (equal rest "")))
      (setq actual (concat actual elem))
      (cond
       (instr
        (cond
         ((equal elem "\"")
          (setq instr (if escape t nil))
          (setq escape nil))
         ((equal elem "\\")
          (setq escape t))))
       ((equal elem "\"")
        (setq instr t))
       ((equal elem "(")
        (setq callcount (+ callcount 1)))
       ((equal elem ")")
        (setq callcount (- callcount 1)))
       ((and (= callcount 0) (equal elem "."))
        (setq result (append result (list (substring actual 0 (- (length actual) 1)))))
        (setq actual "")))

      (if (> (length rest) 0)
          (progn
            (setq elem (substring rest 0 1))
            (setq rest (substring rest 1)))
        (progn
          (setq elem "")
          (setq result (append result (list actual)))))
      )
    result))

;; Todo analyze call expr, to get the types of the calling and all that shiat :)

(defun jmr--clean-expression-left (s)
  (if (string-match "\\`[ \t\n\r\.;)>]+" s)
      (replace-match "" t t s)
    s))

(defun jmr--clean-expression-right (s)
  (if (string-match "[ \t\n\r\.;(<]+\\'" s)
      (replace-match "" t t s)
    s))

(defun jmr--clean-expression (s)
  (jmr--clean-expression-left (jmr--clean-expression-right s)))


(defun jmr--get-actual-package ()
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (re-search-forward "package \\(\\([a-zA-Z0-9]+[.]\\)*[a-zA-Z0-9]+\\)"))
    (when (match-string 1)
      (jmr--strip-text-properties (concat (match-string 1) ".")))))

(provide 'jmr-helper)
;;; jmr-helper.el ends here
