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

;; TODO: to remove, when loaded with require lexical binding will be automatically enabled
(setq lexical-binding t)

(defvar previous-focus nil)

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

(setq my-example (make-instance drop-frame
                          :name "drop down example"
                          :height 600
                          :width 900
                          :top 0
                          :left 300
                          :setup-fn (lambda () (switch-to-buffer (get-buffer-create "drop-frame.el")))))

(setq my-term (make-instance drop-frame
                          :name "drop down terminal"
                          :height 600
                          :width 900
                          :top 0
                          :left 300
                          :setup-fn (lambda ()
                                      (progn
                                        (switch-to-buffer (get-buffer-create "drop-down term"))
                                        (eshell)))))

(create my-example)
(toggle my-example)
(hide my-example)
(show my-example)

(global-set-key (kbd "C-c o e") (lambda () (interactive) (toggle my-example)))
(global-set-key (kbd "C-c o t") (lambda () (interactive) (toggle my-term)))

(provide 'drop-frame)
;;; drop-frame.el ends here
