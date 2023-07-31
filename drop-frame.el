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

(defvar previous-focus nil)

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
(cl-defmethod create ((a-frame drop-frame))
  (let ((focus (selected-frame))
        (fr (make-frame (oref a-frame frame-params))))
        (progn
          (setq previous-focus focus)
          (oset a-frame frame fr)
          (mapc #'funcall (oref a-frame setup-fns)))))

;;;###autoload
(cl-defmethod show ((a-frame drop-frame))
  "toggle the drop-frame"
  (progn
    (setq previous-focus (selected-frame))
    (make-frame-visible (oref a-frame frame))))

;;;###autoload
(cl-defmethod hide ((a-frame drop-frame))
  "toggle the drop-frame"
  (progn
    (select-frame-set-input-focus previous-focus)
    (setq previous-focus nil)
    (make-frame-invisible (oref a-frame frame) t)))

;;;###autoload
(cl-defmethod -toggle ((a-frame drop-frame))
  (if (not (slot-boundp a-frame 'frame))
      (create a-frame)
    (let ((fr (oref a-frame frame)))
      (cond ((or (not fr) (not (frame-live-p fr))) (create a-frame))
            ((frame-visible-p fr) (hide a-frame))
            (:else (show a-frame))))))

;;;###autoload
(defun toggle (a-frame)
  (lambda ()
    (interactive)
    (toggle a-frame)))

(provide 'drop-frame)
;;; drop-frame.el ends here
