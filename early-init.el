;;; early-init.el --- Ran before init.el -*- lexical-binding:t -*-
;;; Commentary:
;;; Mainly just defers gc until the config is loaded
;;; Code:

;;; Defer garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;;; After initialization is done set it to something reasonable
(add-hook 'emacs-startup-hook (lambda ()
				(setq
				 gc-cons-threshold 800000
				 read-process-output-max (* 1024 1024) ;; 1mb
				 )))
