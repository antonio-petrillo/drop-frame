;;; drop-frame.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Antonio Petrillo
;;
;; Author: Antonio Petrillo <antonio.petrillo4@studenti.unina.it>
;; Maintainer: Antonio Petrillo <antonio.petrillo4@studenti.unina.it>
;; Created: July 16, 2023
;; Modified: July 16, 2023
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/antoniopetrillo/drop-frame
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar drop-frame--previous-focus nil)

;;;###autoload
(defclass drop-frame ()
  ((name :initarg :name
         :type string
         :custom string
         :documentation "drop-frame name")
   (frame :type frame
          :documentation "drop-frame frame")
   (frame-params :type list
                 :initform '()
                 :initarg :frame-params
                 :documentation "drop-frame frame parameters")
   (setup-fns :initarg :setup-fns
             :type list
             :documentation "drop-frame setup functions")))

;;;###autoload
(cl-defmethod drop-frame--create ((a-frame drop-frame))
  (let ((focus (selected-frame))
        (fr (make-frame (oref a-frame frame-params))))
        (progn
          (setq drop-frame--previous-focus focus)
          (oset a-frame frame fr)
          (mapc #'funcall (oref a-frame setup-fns)))))

;;;###autoload
(cl-defmethod drop-frame--show ((a-frame drop-frame))
  "toggle the drop-frame"
  (progn
    (setq drop-frame--previous-focus (selected-frame))
    (make-frame-visible (oref a-frame frame))))

;;;###autoload
(cl-defmethod drop-frame--hide ((a-frame drop-frame))
  "toggle the drop-frame"
  (progn
    (select-frame-set-input-focus drop-frame--previous-focus)
    (setq drop-frame--previous-focus nil)
    (make-frame-invisible (oref a-frame frame) t)))

;;;###autoload
(cl-defmethod drop-frame--toggle-private ((a-frame drop-frame))
  (if (not (slot-boundp a-frame 'frame))
      (drop-frame--create a-frame)
    (let ((fr (oref a-frame frame)))
      (cond ((or (not fr) (not (frame-live-p fr))) (drop-frame--create a-frame))
            ((frame-visible-p fr) (drop-frame--hide a-frame))
            (:else (drop-frame--show a-frame))))))

;;;###autoload
(defun drop-frame--toggle (a-frame)
  (lambda ()
    (interactive)
    (drop-frame--toggle-private a-frame)))

(provide 'drop-frame)
;;; drop-frame.el ends here
