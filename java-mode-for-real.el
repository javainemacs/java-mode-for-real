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

(require 'jmr-helper)
(require 'jmr-config)
(require 'jmr-types)
(require 'jmr-exec)
(require 'jmr-complete)

(defgroup jmr nil
  "Java Mode for Real for emacs."
  :prefix "jmr-"
  :group 'programming)

;;; CUSTOM

(defcustom jmr-java-path "/usr/bin/java"
  "Java binary path."
  :type 'string
  :group 'jmr)

(defcustom jmr-javac-path "/usr/bin/javac"
  "Javac binary path."
  :type 'string
  :group 'jmr)

(defcustom jmr-jar-path "/usr/bin/jar"
  "Jar binary path."
  :type 'string
  :group 'jmr)

(defcustom jmr-javap-path "/usr/bin/javap"
  "Javap binary path."
  :type 'string
  :group 'jmr)

;;; VARIABLES

;;; PRIVATE VARIABLES

(defvar-local jmr-cfg nil
  "The config for JMR project of the file.")


;;;###autoload
(defun jmr-version ()
  "Get the version of JMR."
  (interactive)
  (message "JMR %s" jmr-version))

;;; Config API
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

;;; JAR API
;;;###autoload
(defun jmr-add-jar ()
  "Add a jar to the project."
  (interactive)
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
  (with-help-window "Jar list"
    (let ((jarl (append (plist-get jmr-cfg :jars) nil)))
      (when (= (length jarl) 0)
        (princ "No jars!"))
      (-each jarl (lambda (jar)
                    (princ jar)
                    (princ "\n"))))))

;;;###autoload
(defun jmr-delete-jar ()
  "Remove a jar from the project."
  (interactive)
  (let* ((jarv (plist-get jmr-cfg :jars))
         (jarl (append jarv nil))
         (jarp (completing-read "Select a jar file: " jarl)))
    (plist-put
     jmr-cfg :jars
     (vconcat
      (-filter (lambda (n) (not (equal n jarp))) jarl)))
    (jmr--save-cfg jmr-cfg (jmr--check-create-cfg-file))))

;;; Compile API

;;;###autoload
(defun jmr-compile ()
  "Compile current project."
  (interactive)
  (let ((mainp (jmr--get-main-file))
        (prevdd default-directory))
    (cd (jmr--get-src-path))
    (compile (concat jmr-javac-path " -cp \"" (jmr--create-classpah) "\" " mainp))
    (cd prevdd)))

;;; Execute API

(defun jmr--execute-java-auxiliar (mainp)
  (let ((default-directory (jmr--get-src-path)))
    (jmr--execute-java-middleware jmr-java-path mainp)))

;;;###autoload
(defun jmr-execute ()
  "Execute current project main."
  (interactive)
  (jmr--execute-java-auxiliar (jmr--get-main-file)))

;;;###autoload
(defun jmr-execute-custom-main ()
  "Execute other main."
  (interactive)
  (jmr--execute-java-auxiliar (jmr--ask-main-file)))

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

;; Moar things
;;;###autoload
(defun jmr-type-on-pointer ()
  (interactive)
  (message "%s" (jmr--type-on-pointer)))

;;; JMR Mode!

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
