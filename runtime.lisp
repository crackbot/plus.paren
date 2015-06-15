
(in-package :plus.paren)

(defparameter *standard-cl*
  '(progn

    ;; boundp 138
    ;; fboundp 149
    ;; reverse 164
    ;; get 203
    ;; third 258
    ;; nconc 297
    ;; nreverse 362
    ;; make-hash-table 416
    ;; typep 655
    ;; consp 660
    ;; make-array 714
    ;; getf 894
    ;; gethash 1126

    (defun find (item lst &key (test #'eql))
      (find-if (partial test item) lst))
    
    (defun find-if (pred lst)
      (dolist (item lst)
        (when (pred item)
          (return-from find-if item))))
    
    (defun adjoin (item lst &key (test #'eql))
      "Add item to lst unless it's already present"
      (when (not (find item lst))
        (chain lst (push item))))

    (defmacro string-downcase (str)
      `(chain ,str (to-lower-case))) 

    (defmacro string-upcase (str)
      `(chain ,str (to-upper-case)))

    (defmacro nth (idx array)
      `(aref ,array ,idx))

    (defun every (pred seq)
      (eq (length seq)
          (length (find-if pred seq))))

    (defun copy-list (lst)
      (chain lst (slice 0)))

    (defun last (lst)
      (aref lst (- (length lst) 1)))

    (defun remove-if (pred lst)
      (let ((res (array)))
        (dolist (item lst)
          (when (not (pred item))
            (chain res (push item))))
        res))

    (defun count (item seq)
      (length (find item seq)))

    (defun rest (lst)
      (subseq lst 1))

    (defun position (item seq)
      (cond ((arrayp seq)
             (find item seq))
            ((stringp seq)
             (chain seq (index-of item)))
            (t (error ""))))

    ;; for some reason parenscript complains about "test" key
    (defun assoc (item alist &key (tst #'eql))
      (dolist (alist-item alist)
        (when (tst item (aref alist-item 0))
          (return alist-item))))

    ;; assert 820
    (defmacro assert (pred &optional (msg "Assertion failed"))
      `(when (not ,pred)
         (throw (new (*error ,msg)))))

        (defun subseq (seq start &optional (end (length seq)))
      (cond ((arrayp seq)
             (chain seq (slice start end)))
            ((stringp seq)
             (chain seq (substring start end)))
            (t (error ""))))
    
    (defmacro first (lst)
      `(aref ,lst 0))
    
    (defmacro second (lst)
      `(aref ,lst 1))

    (defun memeber (item lst &key test)
      "Check if ITEM is a member of ARR."
      (if (find item lst :test test) t f))

        ;; this are taken from parenscript runtime
    (defun mapcar (fun &rest arrs)
      (let ((result-array (make-array)))
        (if (eq 1 (length arrs))
            (dolist (element (aref arrs 0))
              ((@ result-array push) (fun element)))
            (dotimes (i (length (aref arrs 0)))
              (let ((args-array (mapcar (lambda (a) (aref a i)) arrs)))
                ((@ result-array push) ((@ fun apply) fun args-array)))))
        result-array))

    (defun map-into (fn arr)
      "Call FN on each element in ARR, replace element with the return value."
      (let ((idx 0))
        (dolist (el arr)
          (setf (aref arr idx) (fn el))
          (setf idx (1+ idx))))
      arr)

    (defun map (fn arr)
      "Call FN on each element in ARR and return the returned values in a new array."
      ;; In newer versions of ECMAScript, this may call Array.map, too
      (when (or (not arr) (not (length arr)))
        (return))
      
      (let ((idx 0)
            (result (array)))
        (dolist (el arr)
          (setf (aref result idx) (fn el))
          (setf idx (1+ idx)))
        result))
    
    (defun set-difference (arr arr-to-sub)
      "Return a new array with only those elements in ARR that are not in ARR-TO-SUB."
      (let ((idx 0)
            (result (array)))
        (dolist (el arr)
          (unless (member el arr-to-sub)
            (setf (aref result idx) el)
            (setf idx (1+ idx))))
        result))

    (defun reduce (func list &optional init) ;; the use of init here is actually a bit broken wrt null
      (let* ((acc))
        (do* ((i (if init -1 0) (1+ i))
              (acc (if init init (elt list 0)) (func acc (elt list i))))
             ((>= i (1- (length list)))))
        acc))

    (defun nconc (arr &rest arrs)
      (when (and arr (> (length arr) 0))
        (loop :for other :in arrs :when (and other (> (length other) 0)) :do
           ((@ arr :splice :apply) arr
            (append (list (length arr) (length other)) other))))
      arr)

    (defun maplist (fn arr)
      (when (or (not arr) (not (length arr)))
        (return))

      (let ((res (array)))
        (dotimes (i (length arr))
          (push (fn (slice arr 0 (1+ i))) res))
        res)))
  "Standard common lisp functions ported to parenscript")

(defparameter *plus-library*
  '(progn
    
    (defmacro =|| (var1 def1)
      `(when (not ,var1)
         (setf ,var1 ,def1)))
    
    (defun filter (pred lst)
      (let ((res (array)))
        (dolist (item lst)
          (when (pred item)
            (push item res)))
        res))
    
    (defun/partial pick1 (name obj)
     (getprop obj name))
    
    ;; (defmacro push (item lst)
    ;;   `(chain ,lst (push ,item)))
    
    ;; non CL
    (defmacro push (sym arr)
      `(chain ,arr (push ,sym)))

    ;; (defmacro pop (arr)
    ;;   `(chain ,arr (pop)))

    (defmacro join (sym arr)
      `(chain ,arr (join ,sym)))

    (defmacro except-last (arr)
      `(chain ,arr (slice 0 -1)))

    (defmacro last (arr)
      (let ((var (ps::ps-gensym)))
        `(let ((,var ,arr))
           (getprop ,var (- (length ,var) 1)))))

    (defmacro split (sym arr)
      `(chain ,arr (split ,sym)))
    
    (defmacro slice (arr start &optional end)
      (if end
          `(chain ,arr (slice ,start ,end))
          `(chain ,arr (slice ,start))))

    (defmacro car (arr)
      `(elt ,arr 0))

    (defmacro cdr (arr)
      `(chain ,arr (slice 1)))
    
    ;; (defmacro slice (arr num)
    ;;   `(chain ,arr (slice ,num)))
    
    (defmacro concat (&rest body)
      `(+ ,@body))
    
    (defun nreplace (lst obj1 obj2 &key test)
      "replace obj1 with obj2 inside lst, eq-fun is used to find obj1
   inside lst"
      (let ((idx (find-idx lst obj1 test)))
        (when (numberp idx)
          (setf (aref lst idx) obj2)))
      lst)
    
    (defun find-idx (lst obj eq-fun)
      (setf eq-fun (or eq-fun (lambda (x y)
                                (eq x y))))
      (let ((res nil))
        (do ((idx 0 (1+ idx)))
            ((>= idx (length lst)))
          (when (eq-fun (aref lst idx) obj)
            (setf res idx)
            (return)))
        res))
    
    (defun vals (obj)
      (let ((res (array)))
        (for-in (key obj)
          (push (getprop obj key) res))
        res))

    (defun keys (obj)
      (let ((res (array)))
        (for-in (key obj)
          (push res key))
        res))

    ;; first order functions
    (defun len (val)
      (length val))
    
    (defun onep (val)
      (1+ val))
    
    (defun onem (val)
      (1- val))
  ))
