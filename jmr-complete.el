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

(defun jmr--complete-options (name)
  ;; Check inside string? I don't think so
  (let ((tname (jmr--traverse-expression-types (jmr--clean-expression name))))
    (when tname
      (jmr--analyze-classes tname))))


(provide 'jmr-complete)
;;; jmr-complete.el ends here
