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
  (save-excursion
    (cond
     ((jmr--pointer-in-string)
      "String")
     ((jmr--pointer-inside-call)
      (let (expr tname origp firstp lastp (leftp (jmr--first-unbalanced-left-parenthesys)))
        (setq origp (point))
        ;; Do something like exrp-walker but to the left
        (unless (re-search-backward "[\s]" leftp t nil)
          (goto-char leftp))
        (right-char 1)
        (setq firstp (point))
        (goto-char origp)
        (setq lastp (jmr--expr-walker t nil t))
        (setq expr (jmr--strip-text-properties (buffer-substring firstp lastp)))

        (setq tname (jmr--traverse-expression-types (jmr--clean-expression expr)))
        (when tname
          (message tname)))
      ;; TODO?
      ;; search-left until first non-balanced parenthesis, same for the right
      ;; (jmr--get-current-expression)
      )
     (t
      (let* ((expr (jmr--get-current-expression))
             (tname (jmr--traverse-expression-types (jmr--clean-expression expr))))
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
        (setq calls (list))
        (setq actual nil)))
    lastclass))

(defun jmr--analyze-single-expr-type (expr &optional class)
  (when (> (length expr) 0)
    (cond
     ((string< "Z" (substring expr 0 1))
      ;; lowercase -> variable
      (if (not (null class))
          ;; Search in class
          (jmr--find-property-in-class class expr)
        ;; Search variable
        (jmr--try-get-full-classname (jmr--declared-type-in-body expr))))
     (t
      ;; New class, just get the name (we are not type-checking, only type hinting)
      ;; do the nullClass ?
      (jmr--try-get-full-classname
       (substring expr 0 (or (string-match "[^a-zA-Z0-9_-]" expr) (length expr))))))))

(defun jmr--try-get-full-classname (classname)
  (let ((classes (jmr--analyze-classes classname)))
    (if (and classes (plist-get classes :classname))
        (plist-get classes :classname)
      classname)))

(defun jmr--find-property-in-class (class name)
  (let ((classinfo (jmr--analyze-classes class))
        (isf (string-match-p "(" name)))
    (when classinfo
      (if isf
          (let ((methodinf (jmr--find-method-in classinfo name)))
            (when methodinf
              (plist-get methodinf :return)))
        (let ((varinf (jmr--find-variable-in classinfo name)))
          (when varinf
            (plist-get varinf :type)))))))

;;; We'll probably have some problems when there are more than one with same number & types (example, java.util.Scanner.next() have implementation returning String and Object
(defun jmr--find-method-in (classinfo name)
  (let (fname args nargs mlist)
    (string-match "^\\([^(]+\\)(\\([^)]*\\)" name)
    (setq fname (match-string 1 name))
    (message fname)
    (setq args (match-string 2 name))
    (when (stringp args)
      ;;Remove content of the string
      (setq args (replace-regexp-in-string "\".+\"" "" args)))
    (setq nargs (if (null args) 0 (length (split-string args ","))))
    ;; Analyze every argument, now let's do it only with number of arguments
    (setq mlist (-filter
                 (lambda (elem)
                   (let ((args (or (plist-get elem :args) "")))
                     (and (equal (plist-get elem :name) fname)
                          (equal nargs (if (equal "" args) 0 (length (split-string args ",")))))))
                 (plist-get classinfo :methods)))
    (when (and mlist (> (length mlist) 0))
      ;; From now, return the first one
      (nth 0 mlist))))

(defun jmr--find-variable-in (classinfo name)
  (let (vlist)
    (setq vlist
          (-filter
           (lambda (elem)
             (equal (plist-get elem :name) name))
           (plist-get classinfo :vars)))
    (when (and vlist (> (length vlist) 0))
      (nth 0 vlist))))

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

(defun jmr--validate-type-name (type)
  (and (string-match-p "^[\]\[<>A-Za-z_.0-9]+\\'" type) t))

(defun jmr--javap-execute (class &optional private constants)
  (let* ((classpath (jmr--create-classpah))
         (private (if private "-p " ""))
         (constants (if constants "-constants " ""))
         (shellcmd (concat jmr-javap-path " " private constants " -cp \"" classpath "\" " class))
         (default-directory (jmr--get-src-path)))
    (ignore-errors (shell-command-to-string shellcmd))))

(defun jmr--analyze-javap-line-var (line)
  (let* ((varexpr
         (jmr--clean-expression
          (replace-regexp-in-string "\\(public\\|protected\\|private\\|static\\|final\\|native\\|synchronized\\|abstract\\|transient\\)+\s+" "" line)))
         (splitted (split-string varexpr " "))
         (type (nth 0 splitted)))
    (when (jmr--validate-type-name type)
      (list :name (nth 1 splitted) :type type))
    ))

(defun jmr--analyze-javap-line-method (line classname)
  (let (name args returnt thr gener
        (fexpr
          (jmr--clean-expression
           (replace-regexp-in-string "\\(public\\|protected\\|private\\|static\\|final\\|native\\|synchronized\\|abstract\\|transient\\)+\s+" "" line)))
        (genericr "\\(<\\([A-Za-z0-9,]+\\)>\\)?\s*")
        (typer "\\([^\s]+\\)\s+")
        (namer "\\([^(]+\\)")
        (argsr "(\\([^)]*\\))")
        (throwsr "\\(\s*throws\s+\\(.+\\)\\)?"))
    (string-match (concat genericr typer namer argsr throwsr) fexpr)
    (setq gener (match-string 2 fexpr))              ;Generics(without split in ,)
    (setq returnt (match-string 3 fexpr))              ;ReturnType
    (setq name (match-string 4 fexpr))              ;FName
    (setq args (match-string 5 fexpr))              ;Args(without split in ,)
    (setq thr (match-string 7 fexpr))              ;Throws

    (list
     :name
     name
     :return
     returnt
     :args
     args
     :throws
     thr
     :generic
     gener)))

(defun jmr--analyze-javap-line (line classname)
  (let ((isf (string-match-p "(" line) ;If have ( it's a method!
             (isconstructor (string-match-p classname line))) ;If have classname it's a constructor
        )
    (if (not isconstructor)             ;Decide later what to do with constructors
      (list
       :type
       (if isf :methods :vars)
       :value
       (if isf (jmr--analyze-javap-line-method line classname) (jmr--analyze-javap-line-var line)))
      )))

;; Cache!
(defvar-local jmr--class-cache nil "")

(defun jmr--class-cache-set (key value)
  (when (not jmr--class-cache)
    (setq jmr--class-cache (make-hash-table :test 'equal)))
  (puthash key value jmr--class-cache))

(defun jmr--class-cache-safe-set (key value)
  (when (not jmr--class-cache)
    (setq jmr--class-cache (make-hash-table :test 'equal)))

  (unless (string-match-p (jmr--get-actual-package) key)
    (jmr--class-cache-set key value)))

(defun jmr--class-cache-get (key &optional dft)
  (when (not jmr--class-cache)
    (setq jmr--class-cache (make-hash-table :test 'equal)))
  (gethash key jmr--class-cache dft))

(defun jmr--analyze-class (class)
  (if (not (equal (jmr--class-cache-get class 0) 0))
      (jmr--class-cache-get class)      ;Use let!?
    (let ((outj (jmr--javap-execute class)) methodlist (output (list :classname class :methods (list) :vars (list))))
      (cond
       ((or (null outj) (string-match "^Error:" outj))
        ;; Save error!
        (jmr--class-cache-safe-set class nil)
        nil)

       (t
        (setq methodlist (split-string outj "\n"))
        ;; Get generic names from class header?
        (setq methodlist (-drop 2 methodlist))  ;Remove two first lines ("compiled from" and class header")
        (setq methodlist (-drop-last 2 methodlist))
        (-each
            (--map (jmr--analyze-javap-line it class) methodlist)
          (lambda (el)
            (when (and el (plist-get el :type) (plist-get el :value))
              (plist-put
               output
               (plist-get el :type)
               (append (plist-get output (plist-get el :type)) (list (plist-get el :value)))))
            ))
        (jmr--class-cache-safe-set class output)
        output)))))

(defun jmr--analyze-classes (class)
  (when class
    (let ((ops (if (string-match-p "\\." class) class (jmr--guess-type-of class))))
      (cond
       ((stringp ops)
        ;; Exact class
        (jmr--analyze-class ops))
       ((listp ops)
        ;; Options
        (let (result pack)
          (while (and (not result) (> (length ops) 0))
            (setq pack (concat (nth 0 (nth 0 ops)) class))
            (setq result (jmr--analyze-class pack)) ;First nth for get first possibility, second nth to get package
            (setq ops (cdr ops)))
          result))))))

(provide 'jmr-types)
;;; jmr-types.el ends here
