;;; jmr-exec.el --- execute java helpers

;; Copyright (C) 2016 Rock Neurotiko
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Rock Neurotiko <miguelglafuente AT gmail DOT com>
;; URL: http://github.com/usrname/jmr-exec
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

(defun jmr--execute-java (javacp javamain)
  ;; TODO: Unique name? now "javaexecution"
  (let* ((classpath (jmr--create-classpah))
         (jprocess (start-file-process "Java" "javaexecution" javacp "-cp" classpath javamain)))

    (add-hook 'post-self-insert-hook 'jmr--post-self-insert-hook)

    (switch-to-buffer-other-window (process-buffer jprocess))
    (setq jmr--javaexec t)
    (setq jmr--javaexec-proc jprocess)

    ;; Handle delete (now only works from the end, handle it right!)
    (local-set-key (kbd "DEL")
                   (lambda ()
                     (interactive)
                     (setq jmr--text-to-send (if (> (length jmr--text-to-send) 0) (substring jmr--text-to-send 0 -1) ""))
                     (delete-char -1)
                     ;; (delete-backward-char 1)
                     ))

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
         (remove-hook 'post-self-insert-hook 'jmr--post-self-insert-hook)
         (local-unset-key (kbd "RET"))
         (local-set-key (kbd "q") 'kill-this-buffer)
         (message "Press \"q\" to close the window."))
       ))))

;; (defun jmr--execute-java (javacp javamain)
;;   (let* ((classpath (jmr--create-classpah))
;;          (jprocess (start-file-process "Java" "java" javacp "-cp" classpath javamain)))
;;     (with-help-window "java"
;;       (set-process-sentinel
;;        jprocess
;;        (lambda (proc event)
;;          (switch-to-buffer-other-window (process-buffer proc))
;;          (save-restriction
;;            (let ((inhibit-read-only t)
;;                  (inhibit-point-motion-hooks t))
;;              (insert (concat "----------\Execution " event))))
;;          (goto-char (point-min)))))))

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

(defun jmr--execute-java-middleware (binary mainp)
  (jmr--execute-java binary (jmr--get-class-package-from-path mainp)))

(provide 'jmr-exec)
;;; jmr-exec.el ends here
