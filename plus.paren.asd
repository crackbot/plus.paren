
(defsystem :plus.paren
  :name "plus.paren"
  :description "Utility library for parenscript, useful js functions and ps macros."
  :version "0.0.1"
  :author "Crackbot <thecrackbot@gmail.com>"
  :maintainer "Crackbot <thecrackbot@gmail.com>"
  :license "The MIT License (MIT)"
  :components ((:file "package")
               (:file "runtime")
               (:file "class")
               (:file "mv-fix")
               (:file "anaphorics")
               (:file "main"))
  :depends-on (:parenscript :serve.paren :anaphora :mgl-pax :lisp-unit))
