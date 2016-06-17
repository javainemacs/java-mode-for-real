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


(defun jmr--complete-options (name)
  ;; Check inside string? I don't think so
  (let ((tname (jmr--traverse-expression-types (jmr--clean-expression name))))
    (when tname
      (jmr--analyze-classes tname))))

;; AUTOCOMPLETE

(defvar jmr--source-cache nil)

(ac-clear-variable-after-save 'jmr--source-cache)
(ac-clear-variable-every-minute 'jmr--source-cache)

(defun jmr--source-candidates ()

  ;; Clean Cache if .
  (when (eq ?. (char-before)) (setq jmr--source-cache nil))
  ;; Add all imported
  ;; Add java.lang.* ?
  (if jmr--source-cache jmr--source-cache
    (progn
      ))
  ;; (or jmr--source-cache
  ;;     (setq jmr--source-cache (list "List" "Integer" "String")))
  )

(defvar ac-source-jmr
  '((candidates . jmr--source-candidates)))

;;;###autoload
(defun jmr-set-autocomplete ()
  (interactive)
  ;; Check mode == jmr mode
  (if (equal major-mode 'jmr-mode)
      (progn
        (setq ac-sources '(ac-source-jmr)))
    (message "You are not in JMR mode")))



(provide 'jmr-complete)
;;; jmr-complete.el ends here
