(in-package :npa-utilities)

(defmacro dohash ((key value hash-table &optional result) &body body)
  "Loop over HASH-TABLE with keys and values bound to KEY and VALUE. You can
use NIL in place of a variable name if you don't intend to use the keys or
values."
  (when (null key) (setf key (gensym "KEY")))
  (when (null value) (setf value (gensym "VALUE")))
  (with-gensyms (iterator more?)
    `(with-hash-table-iterator (,iterator ,hash-table)
       (loop (multiple-value-bind (,more? ,key ,value) (,iterator)
               (declare (ignorable ,key ,value))
               (unless ,more? (return ,result))
               (tagbody ,@body))))))

(defun print-hash (hash-table &optional (stream *standard-output*))
  "Print contents of HASH-TABLE."
  (dohash (k v hash-table)
    (format stream "~a  =>  ~a~%" k v)))

(defun split-after-last (predicate list)
  "Return a list of all the items in LIST up to and including the last item
for which PREDICATE was true. Return the remaining items of LIST as a second
list."
  (loop with falses = ()
        for trues on (reverse list)
        for x = (first trues)
        until (funcall predicate x)
        do (push x falses)
        finally (return (values (nreverse trues)
                                falses))))

(defun make-var-idx-pairs (vars)
  "Return a list that is the same as VARS except each element VAR that is not
a list is replaced with the list (VAR G) where G is a gensym."
  (loop for var in vars
        collect (etypecase var
                  (symbol (list var (gensym)))
                  (cons var))))

(defun split-non-indexed (vars)
  "Separate variables to be indexed from those that do not need indices."
  (multiple-value-bind (indexed singles)
      (split-after-last #'consp vars)
    (values (make-var-idx-pairs indexed)
            singles)))

(defmacro dolist<= ((&rest bindings) (list &optional (count-from 0))
                    &body body)
  "Loop over ordered selections of items in LIST, with optional
counters. Each of BINDINGS can be either a symbol, which will be bound to
successive items in LIST, or a pair of symbols, in which case the first is
bound to the elements of LIST and the second to the item's position in LIST
counting from COUNT-FROM."
  (multiple-value-bind (indexed-vars single-vars)
      (split-non-indexed bindings)
    (labels ((single-form (bindings list)
               (if (endp bindings)
                   `(tagbody ,@body)
                   (destructuring-bind (var . bindings) bindings
                     (with-gensyms (tail-name)
                       `(loop for ,tail-name on ,list
                              for ,var = (first ,tail-name)
                              do ,(single-form bindings tail-name))))))
             (indexed-form (bindings old-idx list)
               (if (endp bindings)
                   (single-form single-vars list)
                   (destructuring-bind ((var idx) . bindings) bindings
                     (with-gensyms (tail-name)
                       `(loop for ,tail-name on ,list
                              for ,var = (first ,tail-name)
                              for ,idx upfrom ,old-idx
                              do ,(indexed-form bindings idx tail-name)))))))
      (indexed-form indexed-vars count-from list))))

(defmacro collect-to-list (&body body)
  "Execute BODY forms collecting objects into a list, which is
returned. Within the body, calling (COLLECT ITEM) adds ITEM to the list;
calling (COLLECT-END LIST) sets LIST to be the end of the list and
immediately returns."
  (with-gensyms (list point collect-block item-arg list-arg)
    `(block ,collect-block
       (let* ((,list (list nil))
              (,point ,list))
         (flet ((collect (,item-arg)
                  (setf ,point (setf (rest ,point) (list ,item-arg))))
                (collect-end (&optional (,list-arg ()))
                  (setf (rest ,point) ,list-arg)
                  (return-from ,collect-block (rest ,list))))
           ,@body)
         (rest ,list)))))

(defun maptree (function tree)
  "Call FUNCTION on each item in TREE that is not a list."
  (labels ((walk (tree)
             (unless (null tree)
               (if (consp tree)
                   (progn
                     (walk (car tree))
                     (walk (cdr tree)))
                   (funcall function tree)))))
    (walk tree)))

(defmacro dotree ((var tree &optional result) &body body)
  "Evaluate BODY with VAR bound in turn to each item in TREE."
  `(block nil
     (maptree (lambda (,var) ,@body)
              ,tree)
     ,@(if result `(,result) nil)))
