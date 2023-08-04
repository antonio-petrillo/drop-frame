;;; drop-frame.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Antonio Petrillo
;;
;; Author: Antonio Petrillo <antonio.petrillo4@studenti.unina.it>
;; Maintainer: Antonio Petrillo <antonio.petrillo4@studenti.unina.it>
;; Created: July 16, 2023
;; Modified: August 1, 2023
;; Version: 0.0.4
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/antonio-petrillo/drop-frame
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar drop-frame--previous-focus nil
  "This var store the focus of the frame that open a drop frame, so once the drop frame is hided it could be restored properly.")

(defun drop-frame--show (a-frame)
  "Given a frame make it visible"
  (progn
    (setq drop-frame--previous-focus (selected-frame))
    (make-frame-visible a-frame)))

(defun drop-frame--hide (a-frame)
  "Given a frame make it invisible"
  (progn
    (select-frame-set-input-focus drop-frame--previous-focus)
    (setq drop-frame--previous-focus nil)
    (make-frame-invisible a-frame t)))

(defun drop-frame--toggle (a-frame)
  "Toggle a frame between visible and invisible."
  (if (frame-visible-p a-frame)
      (drop-frame--hide a-frame)
    (drop-frame--show a-frame)))

;; SEE: yequake => yequake--show-buffers
(defun drop-frame--act (setup-action)
  "Function used to setupe a new drop frame with the given actions, stolen from yequake"
  (cl-typecase setup-action
    (string (switch-to-buffer
             (or (get-buffer setup-action)
                 (find-buffer-visiting setup-action)
                 (find-file-noselect setup-action))))
    (function (funcall setup-action))))

;;;###autoload
(defun drop-frame--create (name setup-fns &optional frame-params)
  "Create a new drop frame with the given parameters"
  (let ((new-frame nil))
    (lambda ()
      (interactive)
      (if (and new-frame (frame-live-p new-frame))
          (drop-frame--toggle new-frame)
        (let ((focus (selected-frame))
              (frame-name (concat name " - drop frame")))
          (progn
            (setq new-frame (make-frame frame-params))
            (set-frame-name frame-name)
            (make-frame-visible new-frame)
            (mapc #'drop-frame--act setup-fns)
            (setq drop-frame--previous-focus focus)))))))

(provide 'drop-frame)
;;; drop-frame.el ends here
