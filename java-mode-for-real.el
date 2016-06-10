;;; java-mode-for-real.el --- A real mode for java language.

;; Copyright (C) 2016 Java In Emacs Team
;;
;; Version: 0.0.1
;; Keywords: java mode programming
;; Author: Java In Emacs Team (ACM UPM members: Samuel, Pablo, Luis, Ignacio, Cesar, Adrian, Roberto, Miguel, Victor)
;; URL: http://github.com/javainemacs/java-mode-for-real
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

(defvar jmr-version "0.1"
  "JMR version string.")

(require 'json)
(require 'dash)                         ;https://github.com/magnars/dash.el

(defgroup jmr nil
  "Java Mode for Real for emacs."
  :prefix "jmr-"
  :group 'programming)

;;; CUSTOM

;;; VARIABLES

;;; PRIVATE VARIABLES

(defvar-local jmr-cfg nil
  "The config for JMR project of the file.")
;;;

(defun jmr--strip-text-properties (txt)
  "Remove all the text properties from `TXT'."
  (set-text-properties 0 (length txt) nil txt)
  txt)

;; From JDEE <3

(defun jmr--import-list ()
  "Build the list of java package declared in the current buffer.
It mostly scans the buffer for 'import' statements, and return the
resulting list.  It impliciyly adds the java.lang.* package."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (lst first second)
      (while (not (null
                   (re-search-forward "import[ \t\n\r]+\\(\\([a-zA-Z0-9]+[.]\\)+\\)\\([*]\\|[a-zA-Z0-9]+\\)" nil t) ))
        (setq first (jmr--strip-text-properties (match-string 1)))
        (setq second (jmr--strip-text-properties (match-string 3)))
        (if (string= "*" second)        ;; Maybe load all with jar tf?
            (setq lst (append lst
                              (list (list first second))))
          (setq lst (append (list (list first second))
                            lst))))
      (if (not (member "java.lang.*" lst))
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

(defun jmr--valid-java-declaration-at (point varname)
  "Verify that a `POINT' start a valid java declaration for the `VARNAME' variable."
  (save-excursion
    (goto-char point)
    (if (looking-at (concat "\\([A-Za-z_.]+\\)[ \t\n\r]+" varname "[ \t\n\r]*[;=]"))
        (match-string 1)
      nil)))

;;; Find type in variable declarations (not f declarations)
(defun jmr--declared-type-of (name)
  "Find in the current buffer the java type of the variable NAME.
The function returns a string containing the name of the class, or nil
otherwise.  This function does not give the fully-qualified java class
name, it just returns the type as it is declared."
  (interactive)
  (save-excursion
    (let (found res pos orgpt resname)
      (while (and (not found)
                  (search-backward name nil t))
        (setq pos (point))
        (backward-word 1)
        (setq resname (jmr--valid-java-declaration-at (point) name))
        (goto-char pos)
        (forward-char -1)
        ;; TODO search until function
        ;; Then search from top class nome until first function
        (unless (null resname)
          (setq res resname)
          (setq found t)))
      (jmr--strip-text-properties res))))


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

;; From http://stackoverflow.com/questions/14095189/walk-up-the-directory-tree
(defun jmr--parent-directory (dir)
  "Go to the parent directory of `DIR' unless it's /."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

;;; JMR code

(defun jmr--find-file-in-heirarchy (current-dir fname)
  "Starting from `CURRENT-DIR', search for a file named `FNAME' upwards through the directory hierarchy."
  (let ((file (concat current-dir fname))
        (parent (jmr--parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        file
      (when parent
        (jmr--find-file-in-heirarchy parent fname)))))

(defun jmr--check-create-cfg-file ()
  "Check if .jmr.json exists traversing up the tree.  If don't, ask for a directory and create it."
  (let ((cfgp (jmr--find-file-in-heirarchy (or (buffer-file-name) default-directory) ".jmr.json")))
    (unless cfgp
      ;; .jmr.json don't exist, let's ask to create it!
      (setq cfgp (concat (file-name-directory (read-directory-name "JMR config file couldn't be found, select directory to create it (the base path of the project)")) ".jmr.json"))
      (let ((json-object-type 'plist))
        (write-region (json-encode '(:jars [] :main "" :src "")) nil cfgp nil)))
    cfgp))

;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman 〔zzbba…@aol.com〕”. 2010-09-02
(defun jmr--get-string-from-file (filePath)
  "Return `FILEPATH''s file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun jmr--get-json-from-file (filepath)
  "Return `FILEPATH''s file as json."
  (let* ((filecontent (jmr--get-string-from-file filepath))
         (json-object-type 'plist)
         (jdata (ignore-errors (json-read-from-string filecontent))))
    (unless jdata
      (message "Error reading JSON config file!"))
    jdata))

(defun jmr--safe-get (pl n d)
  "Get from `PL' the `N', if don't exist return `D'."
  (let ((elem (plist-get pl n)))
    (if elem elem d)))

(defun jmr--contains (list elem)
  (let ((cont nil))
    (dolist (x (append list nil))       ;Convert to list
      (when (equal x elem)
        (setq cont t)))
    cont))

(defun jmr--load-cfg-file ()
  "Load JMR config file."
  (let* ((jmrp (jmr--check-create-cfg-file))
         (jmrj (jmr--get-json-from-file jmrp)))

    ;; TODO Load content from jmrp
    (when jmrj
      (message "Using %s JMR config file." jmrp)
      (setq jmr-cfg jmrj))))

(defun jmr--get-cfg-path ()
  (file-name-directory (jmr--check-create-cfg-file)))

(defun jmr--get-src-path ()
  (or (plist-get jmr-cfg :src) (jmr--get-cfg-path)))

(defun jmr--save-cfg (cfg cfgp)
  (write-region (json-encode jmr-cfg) nil cfgp nil))

(defun jmr--ask-for-file (startd msg ext)
  (read-file-name
   msg
   startd nil nil nil
   (lambda (opt)
     (or
      (file-accessible-directory-p opt)  ;it's a directory
      (string= ext (file-name-extension (downcase opt)))))))

(defun jmr--ask-main-file ()
  (jmr--ask-for-file (buffer-file-name) "Choose main file " "java"))

(defun jmr--ask-src-path ()
  (read-directory-name
   "Choose SRC base path: "))

(defun jmr--get-main-file ()
  (let ((main (plist-get jmr-cfg :main)))
    (when (or (null main)
              (string= "" main)
              (not (file-exists-p main))
              (not (string= "java" (file-name-extension (downcase main)))))
      (plist-put jmr-cfg :main (jmr--ask-main-file)))
    (jmr--save-cfg jmr-cfg (jmr--check-create-cfg-file))
    (plist-get jmr-cfg :main)))

(defun jmr--create-classpah ()
  (cl-reduce
   (lambda (x y) (concat x (unless (string= "" x) ":") y))
   (vconcat ["."] (jmr--safe-get jmr-cfg :jars []))
   :initial-value ""))

;; (defun jmr--execute-javac (javacp javamain)
;;   (let ((classpath (jmr--create-classpah)))
;;     (start-file-process "Java Compiler" "javac" javacp "-cp" (concat "\"" classpath "\"") javamain)))

(defvar-local jmr--text-to-send "")
(defvar-local jmr--javaexec nil)
(defvar-local jmr--javaexec-proc nil)

(defun jmr--post-self-insert-hook ()
  (when jmr--javaexec
    (let ((c (string (preceding-char))))
      (cond
       ((equal c (kbd "RET")))
       (t
        (setq jmr--text-to-send (concat jmr--text-to-send c)))))))

(add-hook 'post-self-insert-hook 'jmr--post-self-insert-hook)

(defun jmr--execute-java (javacp javamain)
  ;; TODO: Unique name? now "javaexecution"
  (let* ((classpath (jmr--create-classpah))
         (jprocess (start-file-process "Java" "javaexecution" javacp "-cp" classpath javamain)))

    (switch-to-buffer-other-window (process-buffer jprocess))
    (setq jmr--javaexec t)
    (setq jmr--javaexec-proc jprocess)

    ;; Handle delete (now only works from the end, handle it right!)
    (local-set-key (kbd "DEL")
                   (lambda ()
                     (interactive)
                     (setq jmr--text-to-send (if (> (length jmr--text-to-send) 0) (substring jmr--text-to-send 0 -1) ""))
                     (delete-backward-char 1)))

    ;; Handle enter
    (local-set-key (kbd "RET")
                   (lambda ()
                     (interactive)
                     (newline)
                     (let ((text (concat jmr--text-to-send (kbd "RET"))))
                       (setq jmr--text-to-send "")
                       (when jmr--javaexec-proc
                         (process-send-string jmr--javaexec-proc text)))))

    ;; On C-d (process-send-eof jprocess)
    (local-set-key (kbd "C-d")
                   (lambda ()
                     (interactive)
                     (process-send-eof jmr--javaexec-proc)))

    (set-process-sentinel
     jprocess
     (lambda (proc event)
       (when (equal (buffer-name) (buffer-name (process-buffer proc)))
         (insert (concat "\n----------\Execution " event))
         (read-only-mode)
         (local-unset-key (kbd "RET"))
         (local-set-key (kbd "q") 'kill-this-buffer)
         (message "Press \"q\" to close the window."))
       ))))

(defun jmr--execute-java (javacp javamain)
  (let* ((classpath (jmr--create-classpah))
         (jprocess (start-file-process "Java" "java" javacp "-cp" classpath javamain)))
    (with-help-window "java"
      (set-process-sentinel
       jprocess
       (lambda (proc event)
         (switch-to-buffer-other-window (process-buffer proc))
         (save-restriction
           (let ((inhibit-read-only t)
                 (inhibit-point-motion-hooks t))
             (insert (concat "----------\Execution " event))))
         (goto-char (point-min)))))))

(defun jmr--get-class-package-from-path (fpath)
  ;; path - cfgpath, remove .java, sub / for .

  ;; Open file, read package, add class name. Safer?
  (let (pkn)
    (setq pkn
          (with-temp-buffer
            (insert-file-contents fpath)
            (goto-char (point-min))
            (let ((m ""))
              (ignore-errors (re-search-forward "package \\(\\([a-zA-Z0-9]+[.]\\)*[a-zA-Z0-9]+\\)"))
              (setq m (or (ignore-errors (match-string 1)) ""))
              (setq m (if (string= "" m) "" (concat m ".")))
              (jmr--strip-text-properties m))))

    (concat pkn (file-name-base fpath))))

;;;###autoload
(defun jmr-set-main ()
  "Set main file."
  (interactive)
  (plist-put jmr-cfg :main (jmr--ask-main-file))
  (jmr--save-cfg jmr-cfg (jmr--check-create-cfg-file)))

;;;###autoload
(defun jmr-set-src-path ()
  "Set src path."
  (interactive)
  (plist-put jmr-cfg :src (jmr--ask-src-path))
  (jmr--save-cfg jmr-cfg (jmr--check-create-cfg-file)))

;;;###autoload
(defun jmr-add-jar ()
  "Add a jar to the project."
  (interactive)
  (message "TODO")
  ;; Ask for .jar file (check jmr--ask-main-file)
  (let ((jarp (jmr--ask-for-file (jmr--get-src-path) "Choose jar file " "jar"))
        (jarl (plist-get jmr-cfg :jars)))
    (unless (jmr--contains jarl jarp)
      ;; Add it to jmr-cfg :jars
      (setq jarl (vconcat jarl (vector jarp)))
      (plist-put jmr-cfg :jars jarl)
      ;; Save it
      (jmr--save-cfg jmr-cfg (jmr--check-create-cfg-file)))))

;;;###autoload
(defun jmr-list-jars ()
  "List jar of the project."
  (interactive)
  (message "TODO"))

;;;###autoload
(defun jmr-delete-jar ()
  "Remove a jar from the project."
  (interactive)
  (message "TODO")
  (let* ((jarv (plist-get jmr-cfg :jars))
         (jarl (append jarv nil))
         (jarp (completing-read "Select a jar file: " jarl)))
    (plist-put
     jmr-cfg :jars
     (vconcat
      (-filter (lambda (n) (not (equal n jarp))) jarl)))
    (jmr--save-cfg jmr-cfg (jmr--check-create-cfg-file))))

;;;###autoload
(defun jmr-compile ()
  "Compile current project."
  (interactive)
  (let ((mainp (jmr--get-main-file))
        (prevdd default-directory))
    (cd (jmr--get-src-path))
    (compile (concat "javac " "-cp \"" (jmr--create-classpah) "\" " mainp))
    (cd prevdd)))

;;;###autoload
(defun jmr-execute ()
  "Execute current project main."
  (interactive)
  (let ((mainp (jmr--get-main-file))
        (default-directory (jmr--get-src-path)))
    (jmr--execute-java "/usr/bin/java" (jmr--get-class-package-from-path mainp))))

;;;###autoload
(defun jmr-execute-custom-main ()
  "Execute other main."
  (interactive)
  (let ((mainp (jmr--ask-main-file))
        (default-directory (jmr--get-src-path)))
    (jmr--execute-java "/usr/bin/java" (jmr--get-class-package-from-path mainp))))

;;;###autoload
(defun jmr-compile-execute ()
  (interactive)
  (setq compilation-finish-functions
        (lambda (buffer string)
          (setq compilation-finish-functions nil)
          (cond ((not (string-match "error"  (with-current-buffer buffer (buffer-string))))
                 ;; jmr-cfg is buffer-local
                 ;; let's load the cfg for this buffer (if json is too long rethink this)
                 (jmr--load-cfg-file)
                 (jmr-execute)
                 (kill-buffer buffer)))))
  (jmr-compile))

(derived-mode-init-mode-variables 'jmr-mode)
(put 'jmr-mode 'derived-mode-parent 'java-mode)

;;;###autoload
(defun jmr-mode ()
  "Major mode to be able to build java apps."
  (interactive)
  (java-mode)

  (setq major-mode 'jmr-mode)
  (setq mode-name "JMR")
  (derived-mode-set-keymap 'jmr-mode)
  (derived-mode-set-syntax-table 'jmr-mode)
  (derived-mode-set-abbrev-table 'jmr-mode)

  (jmr--load-cfg-file)

  ;; The next form must be the last executed
  ;; by jmr-mode.
  (derived-mode-run-hooks 'jmr-mode))

(provide 'java-mode-for-real)
;;; java-mode-for-real.el ends here
