;;; gcmh.el --- the Garbage Collector Magic Hack -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020 Andrea Corallo

;; Maintainer: akrl@sdf.org
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
;; Enforce a sneaky Garbage Collection strategy to minimize GC interference with
;; the activity.
;; During normal use a high GC threshold is set.
;; When idling GC is immediately triggered and a low threshold is set.
;; A more detailed explanation of the rationale behind this can be found at
;; http://akrl.sdf.org/

;;; Code:

(defcustom gcmh-low-cons-threshold 800000
  "Low cons GC threshold.
This is the GC threshold used while idling. Default value is the
same of `gc-cons-threshold' default."
  :group 'gcmh
  :type 'number)

(defcustom gcmh-high-cons-threshold #x40000000
  "High cons GC threshold.
This should be set to a value that makes GC unlikely but does not
cause OS paging."
  :group 'gcmh
  :type 'number)

(defcustom gcmh-idle-delay 15
  "Idle time to wait in seconds before triggering GC."
  :group 'gcmh
  :type 'number)

(defcustom gcmh-verbose nil
  "If t, print a message when garbage collecting."
  :group 'gcmh
  :type 'boolean)

(defvar gcmh-idle-timer nil
  "Idle timer for triggering GC.")

(defmacro gcmh-time (&rest body)
  "Measure and return the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun gcmh-set-high-threshold ()
  "Set the high GC threshold.
This is to be used with the `pre-command-hook'."
  (setq gc-cons-threshold gcmh-high-cons-threshold))

(defun gcmh-idle-garbage-collect ()
  "Run garbage collection after `gcmh-idle-delay'."
  (if gcmh-verbose
      (progn
	(message "Garbage collecting...")
	(condition-case-unless-debug e
	    (message "Garbage collecting...done (%.3fs)"
		     (gcmh-time (garbage-collect)))
	  (error (message "Garbage collecting...failed")
		 (signal (car e) (cdr e)))))
    (garbage-collect))
  (setq gc-cons-threshold gcmh-low-cons-threshold))

;;;###autoload
(define-minor-mode gcmh-mode
  "Minor mode to tweak Garbage Collection strategy."
  :lighter " GCMH"
  :global t

  ;; Cancel any pending timer (prevents duplicate idle timers).
  (when (timerp gcmh-idle-timer)
    (cancel-timer gcmh-idle-timer))
  (if gcmh-mode
      (progn
        (setq  gc-cons-threshold gcmh-high-cons-threshold
               ;; When idle for gcmh-idle-delay, run the GC no matter what.
               gcmh-idle-timer (run-with-idle-timer gcmh-idle-delay t
                                                    #'gcmh-idle-garbage-collect))
        ;; Release severe GC strategy before the user restart to working
        (add-hook 'pre-command-hook #'gcmh-set-high-threshold))
    (setq gc-cons-threshold gcmh-low-cons-threshold
          gcmh-idle-timer nil)
    (remove-hook 'pre-command-hook #'gcmh-set-high-threshold)))

(provide 'gcmh)

;;; gcmh.el ends here
