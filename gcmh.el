;;; gcmh.el --- the Garbage Collector Magic Hack -*- lexical-binding:t -*-

;; Copyright (C) 2019 Andrea Corallo

;; Maintainer: andrea_corallo@yahoo.it
;; Package: gcmh
;; Homepage: https://gitlab.com/koral/gcmh
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Keywords: internal

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Enforce an optimized sneaky Garbage Collection strategy to minimize GC
;; interference with the user activity.
;; A more detailed explanation of the rationale behind this can be found at
;; http://akrl.sdf.org/

;;; Code:

(defcustom gcmh-low-cons-threshold gc-cons-threshold
  "High cons gc threshold.
This is the gc threshold used while in idle.  Default value is \
`gc-cons-threshold'"
  :group 'gcmh
  :type 'number)

(defcustom gcmh-high-cons-threshold #x40000000
  "High cons gc threshold.
This should be set to a value that makes GC unlikely but does not make the OS \
paging."
  :group 'gcmh
  :type 'number)

(defcustom gcmh-time-constant 15
  "Idle time to wait in seconds before triggering GC."
  :group 'gcmh
  :type 'number)

(defcustom gcmh-verbose nil
  "If t print a message into when garbage collecting."
  :group 'gcmh
  :type 'boolean)

(defvar gcmh-timer nil
  "Idle timer set for trigering GC.")

(defmacro gcmh-time (&rest body)
  "Measure and return the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun gcmh-set-high-threshold ()
  "Set the high gc thereshold.
This is to be used with the `pre-command-hook'."
  (setq gc-cons-threshold gcmh-high-cons-threshold))

(define-minor-mode gcmh-mode
  "Minor mode tweak Garbage Colelction strategy."
  :lighter " GCMH"
  :require 'gcmh
  :global t
  (cond
   (gcmh-mode
    (progn
      (setq gc-cons-threshold gcmh-high-cons-threshold)
      ;; Print a message when garbage collecting
      (setq garbage-collection-messages gcmh-verbose)
      ;; When idle for 15sec run the GC no matter what.
      (unless gcmh-timer
	(setq gcmh-timer
	      (run-with-idle-timer gcmh-time-constant t
				   (lambda ()
				     (if gcmh-verbose
					 (message "Garbage Collector has run for %.06fsec"
						  (gcmh-time (garbage-collect)))
				       (garbage-collect))
				     (setq gc-cons-threshold gcmh-low-cons-threshold)))))
      ;; Release severe GC strategy before the user restart to working
      (add-hook 'pre-command-hook #'gcmh-set-high-threshold)))
   (t (progn
	(setq gc-cons-threshold gcmh-low-cons-threshold)
	(cancel-timer gcmh-timer)
	(setq gcmh-timer nil)
	(remove-hook 'pre-command-hook #'gcmh-set-high-threshold)))))

(provide 'gcmh)

;;; gcmh.el ends here
