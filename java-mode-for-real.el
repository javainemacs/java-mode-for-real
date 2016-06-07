;;; java-mode-for-real.el --- A real mode for java language.

;; Copyright (C) 2016 Java In Emacs Team
;;
;; Version: 0.0.1
;; Keywords: java mode programming
;; Author: Java In Emacs Team (ACM UPM members: Samuel, Pablo, Luis, Ignacio, Cesar, Adrian, Roberto, Miguel)
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

(defun jmr--strip-text-properties (txt)
  "Remove all the text properties from `TXT'."
  (set-text-properties 0 (length txt) nil txt)
  txt)

;; From jdee
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

;; From http://stackoverflow.com/questions/14095189/walk-up-the-directory-tree
(defun jmr--parent-directory (dir)
  "Go to the parent directory of `DIR' unless it's /."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun jmr--find-file-in-heirarchy (current-dir fname)
  "Starting from `CURRENT-DIR', search for a file named `FNAME' upwards through the directory hierarchy."
  (let ((file (concat current-dir fname))
        (parent (parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        file
      (when parent
        (find-file-in-heirarchy parent fname)))))

(defun jmr--check-create-cfg-file ()
  "Check if .jmr.json exists traversing up the tree.  If don't, ask for a directory and create it."
  (let ((cfgp (jmr--find-file-in-heirarchy (buffer-file-name) ".jmr.json")))
    (unless cfgp
      ;; .jmr.json don't exist, let's ask to create it!
      (setq cfgp (concat (file-name-directory (read-directory-name "JMR config file couldn't be found, select directory to create it (recommended: base path of the project)")) ".jmr.json"))
      (let ((json-object-type 'plist))
        (write-region (json-encode '(:jars [])) nil cfgp nil)))
    cfgp
    ))

(defun jmr--load-cfg-file ()
  "Load JMR config file."
  (let ((jmrp (jmr--check-create-cfg-file)))
    (message "Using %s JMR config file." jmrp)
    ;; TODO Load content from jmrp
    ))

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
