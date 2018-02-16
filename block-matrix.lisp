(in-package :block-matrix)

(defclass block-matrix ()
  ((alist
    :initarg :alist
    :initform (list)
    :accessor block-matrix-alist
    :type list
    :documentation
    "An association list containing the contents of the block matrix. Each
key in ALIST is a block number. Each value is a list of lists each containing
a row index followed by cons cells each containing a column index and
coefficient."))
  (:documentation
   "Sparse block matrix implementation."))

(defun block-matrix (&optional block-number i j (coefficient 1))
  "Create and return a sparse block matrix. If the arguments I, J,
BLOCK-NUMBER, and COEFFICIENTS are supplied the matrix will contain one entry
constructed from these arguments. Otherwise the matrix will be empty."
  (etypecase block-number
    (null (check-type i null)
          (check-type j null)
          (make-instance 'block-matrix))
    (integer (check-type i integer)
             (check-type j integer)
             (make-instance
              'block-matrix
              :alist (list (list block-number
                                 (list i (cons j coefficient))))))))

(defun clear-block-matrix (block-matrix)
  "Set BLOCK-MATRIX to zero, clearing it of all blocks, rows, columns, and
coefficients."
  (setf (block-matrix-alist block-matrix) ()))

(defun block-matrix-p (object)
  "Test if OBJECT is a block matrix."
  (typep object 'block-matrix))

(defun zero-matrix-p (block-matrix)
  "Test if BLOCK-MATRIX is the zero matrix."
  (null (block-matrix-alist block-matrix)))

(defun map-block-matrix-alist (function alist)
  "Apply FUNCTION to each coefficient, block number, and row and column index
in ALIST."
  (dolist (block-list alist)
    (destructuring-bind (block-number . rows) block-list
      (dolist (row-list rows)
        (destructuring-bind (row . columns) row-list
          (dolist (column columns)
            (funcall function
                     (cdr column) block-number row (car column))))))))

(defun map-block-matrix (function block-matrix)
  "Apply FUNCTION to each stored (nonzero) element of BLOCK-MATRIX. FUNCTION
should take four arguments: a coefficient, block number, row index, and
column index, in that order."
  (map-block-matrix-alist function (block-matrix-alist block-matrix)))

(defmacro do-block-matrix ((coefficient block row col block-matrix
                            &optional result)
                           &body body)
  "Evaluate BODY forms with the values of COEFF, BLOCK, ROW, and COL bound to
the coefficients, block numbers, and row and column indices of BLOCK-MATRIX."
  `(block nil
     (map-block-matrix (lambda (,coefficient ,block ,row ,col)
                         (tagbody ,@body))
                       ,block-matrix)
     ,@(if result `(,result) ())))

(defun imap-block-matrix (function interleave block-matrix)
  "Iterate over the elements of BLOCK-MATRIX applying FUNCTION to the
coefficients, block numbers, and row and column indices. Also call INTERLEAVE
between each element."
  (let ((alist (block-matrix-alist block-matrix)))
    (when (null alist) (return-from imap-block-matrix))
    (destructuring-bind (blck (i (j . coeff) . i-rest) . blck-rest)
        (first alist)
      (funcall function coeff blck i j)
      (map-block-matrix-alist
       (lambda (coeff block i j)
         (funcall interleave)
         (funcall function coeff block i j))
       (cond
         (i-rest `((,blck (,i ,@i-rest) ,@blck-rest)
                   ,@(rest alist)))
         (blck-rest `((,blck ,@blck-rest) ,@(rest alist)))
         (t (rest alist)))))))

(defparameter *show-type* nil
  "Show type name in printed representation of block matrices.")

(defmethod print-object ((object block-matrix) stream)
  (flet ((write-bmat ()
           (pprint-logical-block (stream nil)
             (imap-block-matrix
              (lambda (c b i j)
                (funcall (formatter "~d (~d ~d) => ~a") stream b i j c))
              (curry #'pprint-newline :mandatory stream)
              object))))
    (if *print-escape*
        (print-unreadable-object (object stream :type *show-type*)
          (write-bmat))
        (write-bmat))))

(defun assoc<= (number alist)
  "Returns the tail part of ALIST starting with the last entry whose key is
less than or equal to NUMBER. Returns NIL if there is no such key (i.e., the
first key is already greater than NUMBER). ALIST must be a properly
structured association list whose elements are in increasing order of their
keys. The second return value is T if the search stopped because the key was
equal to NUMBER and NIL otherwise."
  (let ((prev nil))
    (loop
      (when (endp alist)
        (return-from assoc<= (values prev nil)))
      (let ((key (car (first alist))))
        (cond
          ((> key number) (return-from assoc<= (values prev nil)))
          ((= key number) (return-from assoc<= (values alist t)))))
      (setf prev alist
            alist (rest alist)))))

(defun advance-to (number list key)
  "Determine if there is an element ELT in LIST for which (FUNCALL KEY ELT)
is equal to NUMBER. If there is then return the tail part of LIST starting
with that element, and NIL otherwise. If the tail is all of LIST then the
second return value is NIL, otherwise it is the cons cell preceding the one
where ELT was found (whose CDR is the tail just described). The elements of
LIST are assumed to be ordered such that (FUNCALL KEY ELT) returns strictly
larger numbers on successive elements in LIST."
  (let ((prev nil))
    (loop
      (when (endp list) (return (values nil nil)))
      (let ((n (funcall key (first list))))
        (when (> n number) (return (values nil nil)))
        (when (= n number) (return (values list prev))))
      (setf prev list
            list (rest list)))))

(defun delete-next-cell (cons)
  "Set CDR of CONS to CDDR of CONS"
  (setf (cdr cons) (cddr cons))
  cons)

(defun delete-cell (list tail before-tail)
  "Return LIST after removing the cons cell at the head of TAIL. TAIL and
BEFORE-TAIL are assumed to be tails of LIST or NIL of the kind returned by
the ADVANCE-TO function."
  (setf tail (rest tail))
  (cond
    ((null before-tail) tail)
    (t (setf (rest before-tail) tail)
       list)))

(defun delete<= (alist numbers)
  "Return nested alist (an alist whose values are nested alists) with nested
item identified by NUMBERS removed."
  (if numbers
      (multiple-value-bind (tail before-tail)
          (advance-to (first numbers) alist #'car)
        (if (null tail)
            alist
            (let ((pair (first tail)))
              (if-let (value (delete<= (cdr pair) (rest numbers)))
                (progn (setf (cdr pair) value)
                       alist)
                (delete-cell alist tail before-tail)))))
      ()))

(defun mdelete (block-matrix block i j)
  "Remove element of indices I, J of block number BLOCK from BLOCK-MATRIX (if
it exists) and return it."
  (setf (block-matrix-alist block-matrix)
        (delete<= (block-matrix-alist block-matrix) (list block i j)))
  block-matrix)

(defun insert-after (item list)
  "Insert ITEM so it becomes the second item of LIST."
  (setf (rest list) (cons item (rest list))))

(defun mcoeff (block-matrix block i j)
  "Find coefficient in BLOCK-MATRIX of block number BLOCK and indices I and
J."
  (let ((alist (block-matrix-alist block-matrix)))
    (or (and (setf alist (assoc-value alist block))
             (setf alist (assoc-value alist i))
             (setf alist (assoc-value alist j)))
        0)))

(defun (setf mcoeff) (coefficient block-matrix block i j)
  "Set coefficient of indices I, J of block number BLOCK in BLOCK-MATRIX to
COEFFICIENT. If COEFFICIENT is zero this removes the corresponding element
from BLOCK matrix."
  (macrolet ((return-case (test &body body)
               `(when ,test ,@body (return-from mcoeff coefficient))))
    (return-case (zerop coefficient)
      (mdelete block-matrix block i j))
    (let ((alist (block-matrix-alist block-matrix)))
      (return-case (null alist)
        (setf (block-matrix-alist block-matrix)
              (list (list block (list i (cons j coefficient))))))
      (multiple-value-bind (tail =?) (assoc<= block alist)
        (return-case (null tail)
          (push (list block (list i (cons j coefficient)))
                (block-matrix-alist block-matrix)))
        (return-case (not =?)
          (insert-after (list block (list i (cons j coefficient))) tail))
        (setf alist (first tail)))
      (multiple-value-bind (tail =?) (assoc<= i (cdr alist))
        (return-case (null tail)
          (insert-after (list i (cons j coefficient)) alist))
        (return-case (not =?)
          (insert-after (list i (cons j coefficient)) tail))
        (setf alist (first tail)))
      (multiple-value-bind (tail =?) (assoc<= j (cdr alist))
        (return-case (null tail)
          (insert-after (cons j coefficient) alist))
        (return-case (not =?)
          (insert-after (cons j coefficient) tail))
        (let ((pair (first tail)))
          (setf (cdr pair) coefficient))))))

(defun mscale (block-matrix scale)
  "Multiply the coefficients in BLOCK-MATRIX by SCALE."
  (do-block-matrix (c b i j block-matrix)
    (setf (mcoeff block-matrix b i j) (* scale c))))

(defun mincrement (block-matrix scale matrix2)
  "Increment BLOCK-MATRIX by SCALE multiplied by MATRIX2. Return the new
BLOCK MATRIX."
  (do-block-matrix (c b i j matrix2)
    (incf (mcoeff block-matrix b i j) (* scale c)))
  block-matrix)

(defun block-numbers (block-matrix)
  "Return a list of the block numbers in BLOCK-MATRIX."
  (loop for (block-number . nil) in (block-matrix-alist block-matrix)
        collect block-number))

(defun block-structure (block-matrices &optional
                                         (base-block 1) (base-index 1))
  "Return a vector of the sizes of the blocks in the list
BLOCK-MATRICES. This is a list of the maximum index in every block for all
the blocks in BLOCK-MATRICES, assuming block numbers start at BASE-BLOCK and
row and column indices start at BASE-INDEX."
  (let ((sizes (make-array 0 :element-type 'integer :adjustable t)))
    (flet ((update-sizes (block-number index)
             (let ((idx (- block-number base-block))
                   (size (1+ (- index base-index))))
               (unless (< idx (length sizes))
                 (adjust-array sizes (1+ idx) :initial-element 0))
               (maxf (aref sizes idx) size))))
      (dolist (matrix block-matrices)
        (dolist (mat-block (block-matrix-alist matrix))
          (destructuring-bind (block-number . rows) mat-block
            (dolist (row rows)
              (destructuring-bind (i . columns) row
                (update-sizes block-number i)
                (dolist (column columns)
                  (update-sizes block-number (car column))))))))
      sizes)))
