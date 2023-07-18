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

(defvar drop-frame-table (make-hash-table :test 'equal))

(defclass drop-frame ()
  ((name :initarg :name
         :type string
         :custom string
         :documentation "drop-frame name")
   (frame :type frame
          :documentation "drop-frame frame")
   (height :initarg :height
           :initform 600
           :type integer
           :documentation "drop-frame frame")
   (width :initarg :width
           :initform 900
           :type integer
           :documentation "drop-frame frame")
   (left :initarg :left
           :initform 300
           :type integer
           :documentation "drop-frame frame")
   (top :initarg :top
           :initform 0
           :type integer
           :documentation "drop-frame frame")
   (setup-fn :initarg :setup-fn
             :type function
             :documentation "drop-frame setup function")))

(cl-defmethod create ((a-frame drop-frame))
  (let ((focus (selected-frame))
        (fr (make-frame (list (cons 'name (oref a-frame name))
                              (cons 'left (oref a-frame left))
                              (cons 'top (oref a-frame top))
                              (cons 'height (cons 'text-pixels (oref a-frame height)))
                              (cons 'width (cons 'text-pixels (oref a-frame width)))))))
        (progn
          (setq previous-focus focus)
          (oset a-frame frame fr)
          (funcall (oref a-frame setup-fn)))))

(cl-defmethod show ((a-frame drop-frame))
  "toggle the drop-frame"
  (progn
    (setq previous-focus (selected-frame))
    (make-frame-visible (oref a-frame frame))))

(cl-defmethod hide ((a-frame drop-frame))
  "toggle the drop-frame"
  (progn
    (select-frame-set-input-focus previous-focus)
    (setq previous-focus nil)
    (make-frame-invisible (oref a-frame frame) t)))

(cl-defmethod toggle ((a-frame drop-frame))
  (interactive)
  (if (not (slot-boundp a-frame 'frame))
      (create a-frame)
    (let ((fr (oref a-frame frame)))
      (cond ((or (not fr) (not (frame-live-p fr))) (create a-frame))
            ((frame-visible-p fr) (hide a-frame))
            (:else (show a-frame))))))

(cl-defun define-new-drop-frame (name setup-fn
                                     &optional
                                      key
                                     (height 600)
                                     (width 900)
                                     (top 0)
                                     (left 300))
  (let* ((new-frame (drop-frame :name name
                                :setup-fn setup-fn
                                :height height
                                :width width
                                :left left
                                :top top))
         (proper-name (replace-regexp-in-string "\\s-+" "-" name))
         (toggle-fn (intern (concat proper-name "-toggle"))))
     (progn
      (puthash proper-name new-frame drop-frame-table)
       (fset toggle-fn `(lambda ()
                          (interactive)
                          (toggle ,new-frame)))
       (if key
           (global-set-key key toggle-fn)))))


(provide 'drop-frame)
;;; drop-frame.el ends here
