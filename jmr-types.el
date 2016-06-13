;;; jmr-types.el --- types helper

;; Copyright (C) 2016 Rock Neurotiko
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Rock Neurotiko <miguelglafuente AT gmail DOT com>
;; URL: http://github.com/usrname/jmr-types
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

(require 'jmr-helper)

(defconst function-regex "\\(public\\|protected\\|private\\|static\\|final\\|native\\|synchronized\\|abstract\\|transient\\)+\s+[$_<>[:word:]]+\s+[$_[:word:]]+\s*\([^\)]*\)\s*{")

(defun jmr--position-previous-method-definition ()
  (save-excursion
    (re-search-backward function-regex nil t nil)))

(defun jmr--position-next-method-definition ()
  (save-excursion
    (re-search-forward function-regex nil t nil)))

(defun jmr--declared-type-in-body (name)
  (save-excursion
    (let (found res pos orgpt resname (fpos (jmr--position-previous-method-definition)))
      (while (and (not found)
                  (re-search-backward (concat "\s" name "[\s;=,)]") nil t)
                  (> (point) (or fpos 0)))
        (setq pos (point))
        ;; (backward-word 1)
        ;; (search-backward "\s" nil t 1)
        (re-search-backward "[^\]\[<>A-Za-z_.0-9]" nil t nil)
        (right-char 1)
        (setq resname (jmr--valid-java-declaration-at (point) name))
        (goto-char pos)
        (forward-char -1)
        (unless (null resname)
          (setq res resname)
          (setq found t)))
      (jmr--strip-text-properties res))))

(defconst class-regex "\\(public\\|protected\\|private\\|final\\|static\\)+\s+class\s+.+{")

(defun jmr--position-previous-class-definition ()
  (save-excursion
    (re-search-backward class-regex nil t nil)))

(defun jmr--declared-type-in-class (name)
  (save-excursion
    (let (found res pos orgpt resname fpos
                (cpos (jmr--position-previous-class-definition)))
      (goto-char (or cpos 0))
      (setq fpos (jmr--position-next-method-definition))

      (while (and (not found)
                  (re-search-forward (concat "\s" name "[\s;=,]") nil t)
                  (< (point) (or fpos (point-max))))
        (setq pos (point))
        (search-backward name nil t 1)
        (search-backward "\s" nil t 1)
        (re-search-backward "[^\]\[><A-Za-z_.0-9]" nil t nil)
        (right-char 1)
        (setq resname (jmr--valid-java-declaration-at (point) name))
        (goto-char pos)
        (forward-char -1)
        (unless (null resname)
          (setq res resname)
          (setq found t)))
      (jmr--strip-text-properties res))))

;;; Find type in 1) method variables and method parameters. 2) class variables
(defun jmr--declared-type-of (name)
  "Find in the current buffer the java type of the variable NAME.
The function returns a string containing the name of the class, or nil
otherwise.  This function does not give the fully-qualified java class
name, it just returns the type as it is declared."
  (interactive)
  (save-excursion
    (let ((inbody  (ignore-errors (jmr--declared-type-in-body name)))
          (inclass (ignore-errors (jmr--declared-type-in-class name))))
      (or inbody inclass))))

;;; Return a name or list of possible packages of a class.
(defun jmr--guess-type-of (name)
  "Guess the fully qualified name of the class NAME, using the import list.
It returns a string if the fqn was found, or a list of possible
packages otherwise."
  (let ((importlist (jmr--import-list)) shortname fullname tmp result)
    (while (and (not (null importlist)) (null result))
      (setq tmp (car importlist))
      (setq shortname (car (cdr tmp)))
      (setq fullname (concat (car tmp) name))
      (cond
       ((string= "*" shortname)
        (setq result importlist))
       ((string= name shortname)
        (setq result fullname))
       (t
        (setq importlist (cdr importlist)))))
    result))

;;; Mirar si dentro de string
;;; Algo solo (hasta principio sin = o ( ), y hasta el final (;) sin = o )
;;; Creacion, a la izquierda un "new", a la derecha hasta ) o ;
;;; Asignacion (hasta = izquierda, derecha hasta ;
(defun jmr--type-on-pointer ()
  (interactive)
  (save-excursion
    (cond
     ((jmr--pointer-in-string)
      "String")
     ((jmr--pointer-inside-call)
      ;; TODO?
      (message "TODO")
      )
     (t
      (let* ((expr (jmr--get-current-expression))
             (tname (jmr--traverse-expression-types (jmr--clean-expression expr)))
             ;; (tname (jmr--declared-type-of (jmr--clean-expression expr)))
             )
        (when tname
          (message tname)))
      ))))

(defun jmr--traverse-expression-types (expr)
  (let ((calls (jmr--split-expression-in-call expr))
        ;; (-map 'jmr--analyze-call-expression calls)
        actual
        lastclass)
    (while (> (length calls) 0)
      (setq actual (nth 0 calls))
      (setq calls (cdr calls))
      (setq lastclass (jmr--analyze-single-expr-type actual lastclass))
      (when (null lastclass)
        ;; analyze failed!
        (setq cdr '())
        (setq actual nil)))
    lastclass))

(defun jmr--analyze-single-expr-type (expr &optional class)
  (when (> (length expr) 0)
    (cond
     ((string< "Z" (substring expr 0 1))
      ;; lowercase -> variable
      (if (not (null class))
          (progn
            ;; Search in class
            )
        (progn
          ;; Search variable
          (jmr--declared-type-of expr)
          ))
      )
     (t
      ;; New class, just get the name (we are not type-checking, only type hinting)
      (substring expr 0 (or (string-match "[^a-zA-Z0-9_-]" expr) (length expr)))))))

;; JAR analyzer

(defun jmr--classpath-to-innerrepr (jarp)
  (let* ((jar (replace-regexp-in-string "/" "." (replace-regexp-in-string "\\.class\\'" "" jarp)))
         (n (string-match "[^.]+\\'" jar)))
    (list (substring jar 0 n) (substring jar n))))

(defun jmr--jar-out-parse (jars)
  (-map
   'jmr--classpath-to-innerrepr
   (-filter
    (lambda (j)
      (let ((splitted (split-string j "/")))
        (and (equal "class" (file-name-extension (downcase j)))
             (not (string-match "\\$" (or (nth (- (length splitted) 1) splitted) ""))) ;for Class$x.class
             )))
    (split-string jars "\n"))))

(defun jmr--analyze-jar (jarp)
  (let ((jars (ignore-errors (shell-command-to-string (concat jmr-jar-path " tf \"" jarp "\"")))))
    (unless (null jars)
      (jmr--jar-out-parse jars)
      )))

;; Javap analyzer

(defun jmr--javap-execute (class &optional private constants)
  (let* ((classpath (jmr--create-classpah))
         (private (if private "-p " ""))
         (constants (if constants "-constants " ""))
         (shellcmd (concat jmr-javap-path " " private constants " -cp \"" classpath "\" " class))
         )
    (ignore-errors (shell-command-to-string shellcmd))))

(defun jmr--analyze-javap-line-var (line)
  (jmr--clean-expression
   (replace-regexp-in-string "\\(public\\|protected\\|private\\|final\\|static\\)+\s+" "" line))
  )

(defun jmr--analyze-javap-line-method (line)
  ""
  )

(defun jmr--analyze-javap-line (line)
  (let ((isf (string-match-p "(" line))) ;If have ( it's a method!
    (:type
     (if isf :methods :vars)
     :value
     (if isf (jmr--analyze-javap-line-method line) (jmr--analyze-javap-line-var line)))
    ))

(defun jmr--analyze-class (class)
  (let ((outj (jmr--javap-execute class)) methodlist (output '(:methods '() :vars '())))
    (when (and outj (not (string-match-p "^Error:" outj)))
      (setq methodlist (split-string outj "\n"))
      (setq methodlist (-drop 2 methodlist))  ;Remove two first lines ("compiled from" and class header")
      (setq methodlist (-drop-last 1 methodlist))
      (-each methodlist
        (lambda (el)
          (when (and el (plist-get el :type) (plist-get el :value))
            (plist-put
             output
             (plist-get el :type)
             (append (plist-get output (plist-get el :type)) (list (plist-get el :value)))))
          ))
      output
      )))

(defun jmr--analyze-classes (class)
  (let ((ops (if (string-match-p "\\." class) class (jmr--guess-type-of class))))
    (cond
     ((stringp ops)
      ;; Exact class
      (jmr--analyze-class class)
      )
     ((listp ops)
      ;; Options
      (let (result pack)
        (while (and (not result) (> (length ops) 0))
          (setq pack (concat (nth 0 (nth 0 ops)) class))
          (setq result (jmr--analyze-class pack)) ;First nth for get first possibility, second nth to get package
          (setq ops (cdr ops)))
        result)
      )
     )
    ))

(provide 'jmr-types)
;;; jmr-types.el ends here
