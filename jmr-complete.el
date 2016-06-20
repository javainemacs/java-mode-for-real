;;; jmr-complete.el --- provide complete helpers

;; Copyright (C) 2016 Rock Neurotiko
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Rock Neurotiko <miguelglafuente AT gmail DOT com>
;; URL: http://github.com/usrname/jmr-complete
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


;; (defun ac-jmr-prefix ()
;;   (message "PREFIX")
;;   (let ((c (char-before)) ret )
;;     (when (eq ?. c)
;;       (setq  ret (point)))
;;     (unless ret
;;       (save-excursion
;;         (skip-chars-backward ".a-z0-9A-Z_\\-")
;;         (setq ret (point))))
;;     ret
;;     ))

;; (defun ac-jmr-action ()
;;   (interactive)
;;   (list "Hola" "Hola2")
;;   )

;; (defun ac-jmr-document (item)
;;   (when (stringp item)
;;     (message item)
;;     ))

;; (defun ac-jmr-candidate ()
;;   ;; (setq )
;;   (list)
;;   )

;; (eval  '(ac-define-source jmr
;;           '((candidates . ac-jmr-candidate)
;;             ;; (candidate-face . ac-php-candidate-face)
;;             ;; (selection-face . ac-php-selection-face)
;;             (prefix . ac-jmr-prefix)
;;             (requires . 0)
;;             (document . ac-jmr-document)
;;             (action . ac-jmr-action)
;;             (cache)
;;             (symbol . "f"))))

(require 'jmr-types)
(require 'jmr-helper)

(require 's)

(defun jmr--complete-options (name)
  (let ((tname (jmr--traverse-expression-types (jmr--clean-expression name))))
    (when tname
      (jmr--analyze-classes tname))))

(defun jmr--complete-file-options ()
  (let ((imported (jmr--import-list)))
    (--filter
     (not (equal "*" it))
     (--map (car (cdr it)) imported))))

(defun jmr--complete-candidates-methods (methods)
  (-map
   (lambda (method)
     (let (summary
           (name (plist-get method :name))
           (args (plist-get method :args))
           (throws (plist-get method :throws))
           (ret (plist-get method :return)))

       (setq summary
             (s-format "$0 $1" 'elt (list (if ret ret "void") (if throws (concat "throws " throws) ""))))
       (setq name (s-format "$0($1)" 'elt (list name (if args args ""))))

       (propertize name 'document args 'summary summary 'symbol "m")
       ))
   methods))

(defun jmr--complete-candidates-vars (vars)
  (-map
   (lambda (var)
     (let ((name (plist-get var :name))
           (typ (plist-get var :type)))
       (propertize name 'summary typ 'symbol "v")))
   vars))

(defun jmr--complete-candidates (expr)
  ;; If expr don't have ., get imported * java.lang.*
  (if (s-contains? "." expr)
      (let* ((nexpr (replace-regexp-in-string "\\.[^\\.]*\\'" "" expr))
             (ops (jmr--complete-options nexpr)))
        (when ops
          (append
           ;; (--map (plist-get it :name) (plist-get ops :methods))
           (jmr--complete-candidates-methods (plist-get ops :methods))
           (jmr--complete-candidates-vars (plist-get ops :vars)))
          ))
    (jmr--complete-file-options)))

;; AUTOCOMPLETE

(defvar jmr--source-cache nil)

(ac-clear-variable-after-save 'jmr--source-cache)
(ac-clear-variable-every-minute 'jmr--source-cache)

(defun jmr--source-candidates ()
  ;; (message "HERE")
  ;; Clean Cache if .
  (when (eq ?. (char-before)) (setq jmr--source-cache nil))
  ;; Add all imported
  ;; Add java.lang.* ?
  (if jmr--source-cache jmr--source-cache
    (let ((expr (jmr--get-expr-since-pointer)))
      (setq jmr--source-cache (jmr--complete-candidates expr))))
  ;; (or jmr--source-cache
  ;;     (setq jmr--source-cache (list "List" "Integer" "String")))
  )

;; (defun jmr--document (item)
;;   (message item))

(defun jmr--document (item)
  ;; (when (stringp item)
  ;;   (message item)
  ;;   )
  nil
  )


;; (defvar ac-source-jmr
;;   '((candidates . jmr--source-candidates)
;;     ;; (document . jmr--document)
;;     ))

(defun jmr--source-action ()
  (message "ACTION")
  ;; Iterate through parameters?
  ;; Remove them or do something like yas?
  )

(defun jmr--source-start ()
  ;; (message "START")
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
        (when (or (eq ?\. c)
                  ;; ->
                  ;; (and (eq ?> c)
                  ;;      (eq ?- (char-before (1- (point)))))
                  ;; ::
                  ;; (and (eq ?: c)
                  ;;      (eq ?: (char-before (1- (point)))))
                  )
          (point)))))

(ac-define-source jmr
  '((candidates . jmr--source-candidates)
    ;; (action . jmr--source-action) ;; Action is called when "TAB" in option
    (prefix . jmr--source-start)
    (requires . 0)
    (document . jmr--document)
    ;; (cache)
    ;; (selection-face . ac-emacs-eclim-selection-face)
    ;; (candidate-face . ac-emacs-eclim-candidate-face)
    (symbol . "f")))

(defun ac-jmr-setup ()
  (add-to-list 'ac-sources 'ac-source-jmr))

;; Save in

;;;###autoload
(defun jmr-set-autocomplete ()
  (interactive)
  ;; Check mode == jmr mode
  (if (equal major-mode 'jmr-mode)
      (progn
        (ac-jmr-setup)
        (add-hook 'jmr-mode-hook 'ac-jmr-setup)
        ;; (setq ac-sources '(ac-source-jmr))
        )
    (message "You are not in JMR mode")))


(provide 'jmr-complete)
;;; jmr-complete.el ends here
