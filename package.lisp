
(defpackage :plus.paren
  (:use :cl :parenscript :anaphora :serve.paren :contracts.paren :mgl-pax)
  (:nicknames "plus")
  (:documentation "Utility library for Parenscript, provides some
  macros, shortcuts and subset of standard common lisp functions.")
  (:export :->
           :create*
           :defun/partial
           :@plus-manual
           :defjsclass
           :defmeta
           :setf%
           :@%
   
           :switch-with-break
           :dolist-idx))
