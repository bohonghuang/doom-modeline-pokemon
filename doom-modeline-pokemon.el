;;; doom-modeline-pokemon.el --- Put animated Pokémon icons on doom-modeline -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Bohong Huang
;;
;; Author: Bohong Huang <1281299809@qq.com>
;; Maintainer: Bohong Huang <1281299809@qq.com>
;; Version: 1.0.0
;; Homepage: https://github.com/bohonghuang/doom-modeline-pokemon
;; Package-Requires: ((cl-lib "0.7.1") (doom-modeline "3.3.3"))
;; SPDX-License-Identifier: GPL3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package provides `doom-modeline-pokemon-mode', which can display animated Pokémon icons on doom modeline.

;;; Code:

(require 'cl-lib)
(require 'doom-modeline)

(cl-defstruct doom-modeline-pokemon
  (number 0 :type integer)
  (form nil :type (or integer null)))

(defvar doom-modeline-pokemon-animation-timer nil)

(defvar doom-modeline-pokemon-major-mode-hash-table nil)

(defcustom doom-modeline-pokemon-icon-path-elements `(,(expand-file-name "icons/" (file-name-directory load-file-name)) "icon" (format "%03d" pokemon-number) (when pokemon-form (format "_%d" (if (numberp pokemon-form) pokemon-form 1))) ".png")
  "Pokémon icon path elements concatenated by `doom-modeline-pokemon-icon-path'."
  :group 'doom-modeline-pokemon
  :type 'sexp)

(defcustom doom-modeline-pokemon-major-mode-alist
  '((fundamental-mode . 151)
    (org-mode . 647)
    (emacs-lisp-mode . 150)
    (inferior-emacs-lisp-mode . (:number 150 :form 1))
    (eshell-mode . (:number 150 :form 2))
    (dired-mode . 369)
    (Custom-mode . 601)
    (lisp-mode . 892)
    (sly-mrepl-mode . (:number 892 :form 1))
    (rust-mode . 99)
    (rustic-mode . 99)
    (emms-playlist-mode . 648)
    (calc-mode . 376)
    (calc-trail-mode . 376)
    (js-mode . 323)
    (java-mode . 324)
    (shell-mode . 366)
    (sh-mode . 366)
    (term-mode . 366)
    (vterm-mode . 494)
    (scala-mode . 851)
    (kotlin-mode . 866)
    (python-mode . 195)
    (dired-mode . 369)
    (eww-mode . 655)
    (typescript-mode . 9)
    (dart-mode . 319)
    (json-mode . 544)
    (c-mode . 60)
    (objc-mode . 61)
    (c++-mode . 62)
    (scheme-mode . 763)
    (makefile-gmake-mode . 128)
    (makefile-bsdmake-mode . 126)
    (cmake-mode . 3)
    (racket-mode . 658)
    (clojure-mode . 623)
    (html-mode . 322)
    (compilation-mode . 534)
    (native-comp-limple-mode . 534)
    (rustic-cargo-run-mode . 534)
    (messages-buffer-mode . 441)
    (backtrace-mode . 770)
    (help-mode . 479)
    (markdown-mode . 39)
    (text-mode . 169)
    (picture-mode . 235)
    (magit-status-mode . 65)
    (mu4e-main-mode . 225)
    (mu4e-view-mode . 225)
    (mu4e-headers-mode . 225)
    (mu4e~update-mail-mode . 225)
    (org-agenda-mode . 437)
    (process-menu-mode . 82))
  "Pokémon definition alist for `major-mode'."
  :group 'doom-modeline-pokemon
  :type '(alist :key-type symbol :value-type sexp)
  :set (lambda (sym val)
         (set-default sym val)
         (setf doom-modeline-pokemon-major-mode-hash-table nil)))

(defcustom doom-modeline-pokemon-number 1008
  "Max number of the available pokemon."
  :group 'doom-modeline-pokemon
  :type 'integer)

(defcustom doom-modeline-pokemon-animated-p 'active
  "Non-nil means turning on the animtion of Pokémon icon."
  :group 'doom-modeline-pokemon
  :type '(choice (const :tag "Active" active)
                 (const :tag "Passive" passive)
                 (const :tag "Disabled" nil)))

(defcustom doom-modeline-pokemon-animation-interval 0.15
  "The animation interval of Pokémon icon."
  :group 'doom-modeline-pokemone
  :type 'float)

(defcustom doom-modeline-pokemon-buffer-name-function #'identity
  "A function that returns buffer name to display Pokémon icon for a buffer."
  :group 'doom-modeline-pokemone
  :type '(choice (const :tag "Identity" identity)
                 (const :tag "Buffer Name" buffer-name)))

(defun doom-modeline-pokemon-icon-path (pokemon)
  "Return icon path for POKEMON."
  (defvar pokemon-number)
  (defvar pokemon-form)
  (let ((pokemon-number (doom-modeline-pokemon-number pokemon))
        (pokemon-form (doom-modeline-pokemon-form pokemon)))
    (apply #'concat (eval `(list . ,doom-modeline-pokemon-icon-path-elements)))))

(defun doom-modeline-pokemon-by-definition (def)
  "Return a `doom-modeline-pokemon' for DEF."
  (pcase def
    ((cl-type integer) (make-doom-modeline-pokemon :number def))
    ((cl-type list) (apply #'make-doom-modeline-pokemon def))
    ((cl-type doom-modeline-pokemon) def)
    (_ (error "Wrong pokemon definition: %s" def))))

(defun doom-modeline-pokemon-by-major-mode (&optional mode)
  "Return a `doom-modeline-pokemon' for MODE."
  (unless doom-modeline-pokemon-major-mode-hash-table
    (let ((ht (make-hash-table)))
      (dolist (elem doom-modeline-pokemon-major-mode-alist)
        (puthash (car elem) (cdr elem) ht))
      (setf doom-modeline-pokemon-major-mode-hash-table ht)))
  (let* ((mode (or mode major-mode))
         (def (gethash mode doom-modeline-pokemon-major-mode-hash-table)))
    (doom-modeline-pokemon-by-definition (or def (doom-modeline-pokemon-by-object (symbol-name mode))))))

(defun doom-modeline-pokemon-by-object (object)
  "Return a `doom-modeline-pokemon' for OBJECT."
  (make-doom-modeline-pokemon :number (1+ (mod (sxhash object) doom-modeline-pokemon-number))))

(defun doom-modeline-pokemon-icon (pokemon &optional text)
  "Propertize TEXT with the icon of POKEMON."
  (propertize (or text " ") 'display
              `((slice ,(if doom-modeline-pokemon-animated-p (* (% (truncate(/ (float-time) doom-modeline-pokemon-animation-interval)) 2) 0.5) 0.0) 0.0 0.5 1.0)
                (image :type png :file ,(doom-modeline-pokemon-icon-path pokemon) :ascent center))))

(defun doom-modeline-pokemon-buffer-mode-icon ()
  "Return the Pokémon icon for the `major-mode' of current buffer."
  (doom-modeline-pokemon-icon (doom-modeline-pokemon-by-major-mode)))

(defun doom-modeline-pokemon-buffer-name-icon ()
  "Return the Pokémon icon for the name of current buffer."
  (doom-modeline-pokemon-icon
   (doom-modeline-pokemon-by-object
    (funcall doom-modeline-pokemon-buffer-name-function (current-buffer)))))

(doom-modeline-def-segment buffer-info-pokemon
  "Combined information along with Pokémon icons for current buffer."
  (concat
   (doom-modeline-pokemon-buffer-mode-icon)
   (doom-modeline-spc)
   (doom-modeline--buffer-state-icon)
   (doom-modeline--buffer-name)
   (doom-modeline-spc)
   (doom-modeline-pokemon-buffer-name-icon)))

(doom-modeline-def-segment buffer-default-directory-pokemon
  "Displays `default-directory' with the Pokémon icon and state."
  (let ((face (doom-modeline-face
               (if (and buffer-file-name (buffer-modified-p))
                   'doom-modeline-buffer-modified
                 'doom-modeline-buffer-path))))
    (concat (and doom-modeline-major-mode-icon
                 (concat (doom-modeline-pokemon-buffer-mode-icon)
                         (doom-modeline-vspc)))
            (doom-modeline--buffer-state-icon)
            (propertize (abbreviate-file-name default-directory) 'face face))))

(define-minor-mode doom-modeline-pokemon-mode
  "Minor mode to put Pokémon icons on `doom-modeline'."
  :global t
  :group 'doom-modeline-pokemon
  (if doom-modeline-pokemon-mode
      (progn
        (advice-add #'doom-modeline-segment--buffer-info :override #'doom-modeline-segment--buffer-info-pokemon)
        (advice-add #'doom-modeline-segment--buffer-default-directory :override #'doom-modeline-segment--buffer-default-directory-pokemon)
        (when (eq doom-modeline-pokemon-animated-p 'active)
          (setf doom-modeline-pokemon-animation-timer (run-at-time t doom-modeline-pokemon-animation-interval #'force-mode-line-update))))
    (advice-remove #'doom-modeline-segment--buffer-info #'doom-modeline-segment--buffer-info-pokemon)
    (advice-remove #'doom-modeline-segment--buffer-default-directory #'doom-modeline-segment--buffer-default-directory-pokemon)
    (when doom-modeline-pokemon-animation-timer
      (cl-callf cancel-timer doom-modeline-pokemon-animation-timer))))

(provide 'doom-modeline-pokemon)
;;; doom-modeline-pokemon.el ends here
