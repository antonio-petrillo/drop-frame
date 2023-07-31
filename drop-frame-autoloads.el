;;; drop-frame-autoloads.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Antonio Petrillo
;;
;; Author: Antonio Petrillo <antonio.petrillo4@studenti.unina.it>
;; Maintainer: Antonio Petrillo <antonio.petrillo4@studenti.unina.it>
;; Created: July 31, 2023
;; Modified: July 31, 2023
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/antoniopetrillo/drop-frame-autoloads
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))
(autoload 'drop-frame "drop-frame" "drop frame" nil t)

(provide 'drop-frame-autoloads)
;;; drop-frame-autoloads.el ends here
