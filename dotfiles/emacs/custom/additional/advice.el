;;; advice.el --- Helper functions for working with advice.
;; Author: 
;;; Commentary:

;;; Code

(defun list-advices (fn)
  "Return a list of advices added to FN."
  (let (fns)
    (advice-mapc 
     (lambda (f &rest rest)
       (setq fns (append fns (list f))))
     fn)
    fns))
       
(defun remove-all-advice (fn)
  "Remove all advice from FN."
  (advice-mapc
   (lambda (f &rest rest)
     (advice-remove fn f))
   fn))
