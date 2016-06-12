;;; jmr-config.el --- Config methods helpels

;; Copyright (C) 2016 Rock Neurotiko
;;
;; Version: 0.0.1
;; Keywords:
;; Author: Rock Neurotiko <miguelglafuente AT gmail DOT com>
;; URL: http://github.com/usrname/jmr-config
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

;; From http://stackoverflow.com/questions/14095189/walk-up-the-directory-tree
(defun jmr--parent-directory (dir)
  "Go to the parent directory of `DIR' unless it's /."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

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


(provide 'jmr-config)
;;; jmr-config.el ends here
