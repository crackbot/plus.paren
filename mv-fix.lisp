
(in-package :plus.paren)

(defpsmacro values (&optional main &rest additional)
  (when main
    (if additional
        (with-ps-gensyms (val1 valrest)
          `(let ((,val1 ,main)
                 (,valrest (list ,@additional)))
             (when (defined (@ arguments :callee :caller :mv))
               (setf (@ arguments :callee :caller :mv) ,valrest))
             ,val1))
        main)))

(defpsmacro multiple-value-bind (vars form &body body)
  (let* ((form (ps::ps-macroexpand form))
         (progn-form
          (when (and (consp form)
                     (member
                      (car form)
                      '(with label let flet labels macrolet symbol-macrolet progn)))
            (pop form))))
    (with-ps-gensyms (mv prev-mv)
      `(let (,prev-mv)
         (,(or progn-form 'progn)
          ,@(when progn-form (butlast form))
          (setf ,prev-mv (@ arguments :callee :mv))
          (try
           (progn
             (setf (@ arguments :callee :mv) t)
             (let ((,(car vars) ,(if progn-form (car (last form)) form))
                   (,mv (if (objectp (@ arguments :callee :mv))
                            (@ arguments :callee :mv)
                            (make-array ,(1- (length vars))))))
               (destructuring-bind ,(cdr vars) ,mv
                 ,@body)))
           (:finally (if (undefined ,prev-mv)
                         (delete (@ arguments :callee :mv))
                         (setf (@ arguments :callee :mv) ,prev-mv)))))))))
