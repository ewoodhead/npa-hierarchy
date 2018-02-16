(in-package :operators)

(defclass monomial ()
  ((sites
    :initarg :sites
    :initform ()
    :reader monomial-sites
    :type list))
  (:documentation
   "Representation of a monomial (e.g., A1|1 B3|2) as a list of sites and
projectors at those sites, contained in the slot SITES. Each element of SITES
should be a list whose first element is a site number and whose remaining
elements are cons cells containing output numbers (in the cars) and input
numbers (in the cdrs). For example, ((0 (1 . 1) (1 . 2)) (2 (2 . 2))) is a
valid sites list, representing the monomial A1|1 A1|2 C2|2.

Various functions that work with this class assume:
  1) The site numbers are real numbers. (The multiplication functions assume
     they can be compared with < and >.)
  2) The sites are listed in order, with the smallest site number first, and
     with no site number appearing more than once.
  3) The list of projectors is already in its simplest form, i.e., the same
     projector does not appear more than once in a row and orthogonal
     projectors (projectors with the same input number and different output
     numbers, which would make the entire monomial zero) never appear next to
     each other.

Functions that convert momonials to strings additionally assume that the site
numbers are nonnegative integers. Functions that construct monomials from
strings always construct monomials where all the site, output, and input
numbers are nonnegative integers.

The main purpose of this class is to give monomials a type distinct from
LIST. In particular this allows them to have their own PRINT-OBJECT
method. It's a structure rather than a class so that monomials can be used as
hash table keys."))

(declaim (ftype (function (monomial) list) monomial-sites))
(declaim (ftype function write-monomial))

(defmethod print-object ((object monomial) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type nil)
        (pprint-logical-block (stream nil)
          (write-monomial object stream)))
      (pprint-logical-block (stream nil)
        (write-monomial object stream))))

(defun make-monomial (&optional (sites ()))
  "Create a monomial with sites list SITES."
  (declare (list sites))
  (make-instance 'monomial :sites sites))

(defun monomial-p (object)
  "Test if OBJECT is a monomial."
  (typep object 'monomial))

(defparameter *id* (make-monomial)
  "Monomial representing the identity.")

(defvar *default-output* 1
  "Default output number to use.")

(defvar *default-input* 1
  "Default input number to use.")

(defun projector (&optional site output-or-input input)
  "Create a monomial object representing the identity or a single projector.
If SITE is omitted the identity is returned. If OUTPUT-OR-INPUT is omitted
the values *DEFAULT-OUTPUT* and *DEFAULT-INPUT* are used as the output and
input numbers. If INPUT is omitted *DEFAULT-OUTPUT* is used as the output
number and OUTPUT-OR-INPUT is used as the input number. Otherwise
OUTPUT-OR-INPUT is used as the output number and INPUT is used as the input
number."
  (cond
    ((null site) (return-from projector *id*))
    ((null output-or-input) (setf output-or-input *default-output*
                                  input *default-input*))
    ((null input) (setf input output-or-input
                        output-or-input *default-output*)))
  (make-monomial (list (list site (cons output-or-input input)))))

(defun probability (&rest outputs-and-inputs)
  "Construct the monomial corresponding to the probability at the lowest
numbered sites for given outputs and inputs. The outputs should be listed
first, followed by the inputs. For example, (PROBABILITY 1 2 3 4) returns the
monomial A1|3 B2|4."
  (let ((len (list-length outputs-and-inputs)))
    (if (evenp len)
        (make-monomial
         (loop with nsites = (/ len 2)
               for x in (nthcdr nsites outputs-and-inputs)
               for outputs = outputs-and-inputs then (rest outputs)
               for a = (first outputs)
               for site upfrom 0
               collect (list site (cons a x))))
        (error "Even number of arguments expected."))))

(setf (symbol-function 'p) #'probability)

(declaim (ftype function char-ord))
(declaim (special *alphabet*)
         (simple-string *alphabet*))

(defun monomial-hash (monomial)
  "Return a hash code for MONOMIAL. This version does a simple conversion of
MONOMIAL to a string and then uses the default string hashing function. In
SBCL this seems to result in fast hashing for monomials."
  (sxhash
   (with-output-to-string (str)
     (loop for (s . p) in (monomial-sites monomial)
           do (write-char (code-char s) str)
              (loop for (a . x) in p
                    do (write-char (code-char a) str)
                       (write-char (code-char x) str))))))

(declaim (ftype function monomial=))

(defun make-monomial-hash-table ()
  "Make a hash table intended to be used with monomials as keys."
  (make-hash-table :test #+sbcl #'monomial= #-sbcl #'equalp
                   #+sbcl :hash-function #+sbcl #'monomial-hash))

(defun monomial-hash-table (&rest initial-values-and-keys)
  "Return a hash table intended to be used with monomials as keys, optionally
containing initial values and keys given as arguments."
  (let ((table (make-monomial-hash-table)))
    (loop for (value key) on initial-values-and-keys by #'cddr
          when (null key) do (error "missing key in arguments")
          do (setf (gethash key table) value)
          finally (return table))))

(defun copy-monomial-hash-table (table)
  "Return a (shallow) copy of monomial hash table TABLE."
  (let ((result (make-monomial-hash-table)))
    (dohash (mon cf table)
      (setf (gethash mon result) cf))
    result))

(defun reverse-join (list1 list2)
  "Add the elements of LIST1 in reverse order to the beginning of LIST2."
  (let ((result list2))
    (dolist (item list1 result)
      (push item result))))

(defun site-c* (p q)
  "Conjugate multiplication of lists of projectors Q and P at a given site. P
and Q should be non-empty lists of cons cells (a . x) each representing an
output-input pair a and x. This function returns a list consisting of the
contents of P added in reverse order to Q, with the following exceptions: if
the first cons cells of both P and Q represent the same output and input then
the first cell of P is dropped and only the rest of Q is appended in reverse
order (because they represent the same projector, and a projector squared is
itself); if the first cells represent the same input and different outputs
then 0 is returned (because they represent orthogonal projectors, whose
product is zero)."
  (destructuring-bind (ap . xp) (first p)
    (destructuring-bind (aq . xq) (first q)
      (if (= xp xq)
          (if (= ap aq)
              (reverse-join (rest p) q)
              0)
          (reverse-join p q)))))

(defun site* (p q)
  "Multiplication of lists P and Q of cons cells representing projectors at a
given site. This is the same as (SITE-C* P* Q) if P* is P in reverse order."
  (site-c* (reverse p) q))

(defun sites-conj (sites)
  "Conjugate (reverse) the list of projectors at each site in SITES."
  (loop for (site . projectors) in sites
        collect (cons site (reverse projectors))))

(defun order (monomial)
  "Compute the order (number of projectors) of MONOMIAL. The identity has
order 0."
  (loop for (nil . projectors) in (monomial-sites monomial)
        sum (list-length projectors)))

(defun projector< (p q)
  "Test if projector P lexically precedes projector Q. P and Q should be cons
cells each containing an output (car) and input (cdr)."
  (destructuring-bind (a . x) p
    (destructuring-bind (b . y) q
      (or (< x y)
          (and (= x y) (< a b))))))

(defun projector> (p q)
  "Test if projector Q lexically precedes projector P. P and Q should be cons
cells each containing an output (car) and input (cdr)."
  (destructuring-bind (a . x) p
    (destructuring-bind (b . y) q
      (or (> x y)
          (and (= x y) (> a b))))))

(defun monomial= (x y)
  "Test if X and Y represent the same monomial."
  (let ((sites-x (monomial-sites x))
        (sites-y (monomial-sites y)))
    (loop
      (when (null sites-x) (return-from monomial= (null sites-y)))
      (when (null sites-y) (return-from monomial= nil))
      (let ((site-x (first sites-x))
            (site-y (first sites-y)))
        (unless (= (car site-x) (car site-y)) (return nil))
        (let ((projectors-x (cdr site-x))
              (projectors-y (cdr site-y)))
          (loop
            (let ((projector-x (first projectors-x))
                  (projector-y (first projectors-y)))
              (unless (and (= (car projector-x) (car projector-y))
                           (= (cdr projector-x) (cdr projector-y)))
                      (return-from monomial= nil)))
            (setf projectors-x (rest projectors-x)
                  projectors-y (rest projectors-y))
            (when (null projectors-x)
              (if (null projectors-y)
                  (return)
                  (return-from monomial= nil)))
            (when (null projectors-y) (return-from monomial= nil)))))
      (setf sites-x (rest sites-x)
            sites-y (rest sites-y)))))

(defun monomial/= (x y)
  "Test if X and Y are different monomials."
  (not (monomial= x y)))

(defun monomial-cmp (x y if=)
  "Returns true if monomial X lexically precedes monomial Y, false if Y
lexically precedes X, and IF= otherwise."
  (macrolet ((return-if (test-t test-nil)
               `(progn (when ,test-t (return-from monomial-cmp t))
                       (when ,test-nil (return-from monomial-cmp nil)))))
    (let ((ox (order x))
          (oy (order y)))
      (return-if (< ox oy) (> ox oy)))
    (loop for (site-x . projectors-x) in (monomial-sites x)
          for (site-y . projectors-y) in (monomial-sites y)
          do (return-if (< site-x site-y) (> site-x site-y))
             (let ((nprojectors-x (length projectors-x))
                   (nprojectors-y (length projectors-y)))
               (return-if (> nprojectors-x nprojectors-y)
                          (< nprojectors-x nprojectors-y)))
             (loop for p in projectors-x
                   for q in projectors-y
                   do (return-if (projector< p q) (projector> p q)))
          finally (return if=))))

(defun monomial< (x y)
  "Test if monomial X lexicographically precedes monomial Y."
  (monomial-cmp x y nil))

(defun monomial<= (x y)
  "Test if monomial X lexicographically precedes or represents the same
monomial as Y."
  (monomial-cmp x y t))

(defun monomial> (x y)
  "Test if monomial Y lexicographically precedes monomial X."
  (monomial-cmp y x nil))

(defun monomial>= (x y)
  "Test if monomial Y lexicographically predeces or represents the same
monomial as X."
  (monomial-cmp y x t))

(defun monomial-max (monomial &rest more-monomials)
  "Return the monomial that is equal to or lexicographically follows all the
other arguments."
  (check-type monomial monomial)
  (dolist (m more-monomials)
    (when (monomial> m monomial)
      (setf monomial m)))
  monomial)

(defun monomial-min (monomial &rest more-monomials)
  "Return the monomial that is equal to or lexicographically precedes all the
other arguments."
  (check-type monomial monomial)
  (dolist (m more-monomials)
    (when (monomial< m monomial)
      (setf monomial m)))
  monomial)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun monomial-loop-expansion (site output input monomial result body)
    "Generate expansion for the DO-SITES and DO-PROJECTORS macros."
    (when (and (null site) (or output input))
      (setf site (gensym "SITE")))
    (with-gensyms (sites site-list projectors sites-loop sites-test
                         projectors-loop projectors-test)
      (let ((projector (if (or output input) (gensym "PROJECTOR") ())))
        `(let (,@(remove nil `(,site ,output ,input ,projector))
               ,site-list ,projectors
                 (,sites (monomial-sites ,monomial)))
           (block nil
             (tagbody
                (go ,sites-test)
                ,sites-loop
                (setf ,site-list (first ,sites)
                      ,@(if site `(,site (car ,site-list)) ())
                      ,projectors (cdr ,site-list))
                ,projectors-loop
                ,@(if projector
                      `((setf ,projector (first ,projectors)
                              ,@(if output `(,output (car ,projector)))
                              ,@(if input `(,input (cdr ,projector))))))
                ,@body
                (setf ,projectors (rest ,projectors))
                ,projectors-test
                (unless (null ,projectors) (go ,projectors-loop))
                (setf ,sites (rest ,sites))
                ,sites-test
                (unless (null ,sites) (go ,sites-loop)))
             ,@(if result `(,result) nil)))))))

(defmacro do-sites ((site-name output-name input-name monomial
                     &optional result)
                    &body body)
  "Loop with SITE-NAME, OUTPUT-NAME, and INPUT-NAME bound to the site,
output, and input numbers of projectors in MONOMIAL. You can use NIL for
values you don't want to bind."
  (monomial-loop-expansion
   site-name output-name input-name
   monomial result `((tagbody ,@body))))

(defmacro do-projectors ((var monomial &optional result) &body body)
  "Loop with VAR bound to each projector in MONOMIAL."
  (with-gensyms (site output input)
    `(let (,var)
       ,(monomial-loop-expansion
         site output input
         monomial result
         `((setf ,var (projector ,site ,output ,input))
           (tagbody ,@body))))))

(defun monomial* (x y site-mul x-projs-tfm x-tail-tfm)
  "Skeleton function for multiplication of two monomials. The arguments X and
Y should be monomials. SITE-MUL, X-PROJS-TFM, and X-TAIL-TFM should be
functions. They are used in the following way to determine a product:
  SITE-MUL should accept two lists of projectors (cons cells each containing
    an output and input) and return a list of projectors or the integer 0. It
    is applied to projector lists appearing at the same site in both X and
    Y.
  X-PROJS-TFM should accept a list of projectors and return a list of
    projectors. It is applied to determine the list of projectors at sites
    appearing in X but not in Y.
  X-TAIL-TFM should accept a list of sites (cons cells each containing a site
    number and list of projectors, i.e., the same kind of list as the
    MONOMIAL structure's SITES slot) and return a list of sites. It is
    applied to the list of sites in X of site numbers higher than the highest
    site number in Y, if there are any.
For example, to implement multiplication of the conjugate of X with Y, these
functions should implement the required reversing of the order of projectors
that will be applied to sites and projectors from X."
  (make-monomial
   (collect-to-list
    (let ((sites-x (monomial-sites x))
          (sites-y (monomial-sites y)))
      (loop
        (cond
          ((endp sites-x) (collect-end sites-y))
          ((endp sites-y) (collect-end (funcall x-tail-tfm sites-x)))
          (t (destructuring-bind (site-x . projectors-x) (first sites-x)
               (destructuring-bind (site-y . projectors-y) (first sites-y)
                 (cond
                   ((< site-x site-y)
                    (collect (cons site-x
                                   (funcall x-projs-tfm projectors-x)))
                    (setf sites-x (rest sites-x)))
                   ((> site-x site-y)
                    (collect (cons site-y projectors-y))
                    (setf sites-y (rest sites-y)))
                   (t
                    (let ((projectors (funcall site-mul
                                               projectors-x projectors-y)))
                      (when (eql projectors 0)
                        (return-from monomial* 0))
                      (collect (cons site-x projectors)))
                    (setf sites-x (rest sites-x)
                          sites-y (rest sites-y)))))))))))))

(defclass polynomial ()
  ((terms
    :initarg :terms
    :initform (make-monomial-hash-table)
    :reader polynomial-terms
    :type hash-table
    :documentation
    "A hash table mapping monomials (the hash keys) to nonzero coefficients
(the hash values)."))
  (:documentation
   "A polynomial (a linear combination of monomials, e.g. A1|1 + 2 B1|2),
internally represented as a hash table mapping monomials to coefficients."))

(declaim (ftype function write-polynomial))

(defmethod print-object ((object polynomial) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t)
        (pprint-logical-block (stream nil)
          (write-polynomial object stream)))
      (pprint-logical-block (stream nil)
        (write-polynomial object stream))))

(defun polynomial-p (object)
  "Return T if OBJECT is a polynomial and NIL if not."
  (typep object 'polynomial))

(defun polynomial (&optional scalar-or-operator monomial-or-site
                     output-or-input input)
  "Create a POLYNOMIAL object. If called without any arguments, returns the
zero polynomial. If SCALAR-OR-OPERATOR is a polynomial it is returned. If
SCALAR-OR-OPERATOR is a monomial, returns a polynomial containing just that
monomial multiplied by 1. If SCALAR-OR-OPERATOR is a number and it is
nonzero, returns that number multiplied by:
  * The identity if the remaining arguments are not supplied.
  * MONOMIAL-OR-SITE if this variable is a monomial.
  * The monomial constructed by calling
    (PROJECTOR MONOMIAL-OR-SITE OUTPUT-OR-INPUT INPUT) if MONOMIAL-OR-SITE
    is a number."
  (when (polynomial-p scalar-or-operator)
    (return-from polynomial scalar-or-operator))
  (let ((terms (make-monomial-hash-table)))
    (etypecase scalar-or-operator
      (null)
      (monomial (setf (gethash scalar-or-operator terms) 1))
      (number
       (unless (zerop scalar-or-operator)
         (typecase monomial-or-site
           (monomial (setf (gethash monomial-or-site terms)
                           scalar-or-operator))
           (t (setf (gethash (projector monomial-or-site
                                        output-or-input
                                        input)
                             terms)
                    scalar-or-operator))))))
    (make-instance 'polynomial :terms terms)))

(defun copy-polynomial (polynomial)
  "Return a copy of POLYNOMIAL, with the same monomials."
  (make-instance 'polynomial
                 :terms (copy-monomial-hash-table
                         (polynomial-terms polynomial))))

(defun fresh-polynomial (object)
  "Return a fresh polynomial from OBJECT. If OBJECT is a number or monomial
this constructs a corresponding polynomial object and returns it. If OBJECT
is a polynomial then this makes a copy of OBJECT and returns it."
  (if (polynomial-p object)
      (copy-polynomial object)
      (polynomial object)))

(defun remove-monomial (monomial polynomial)
  "Remove MONOMIAL and its coefficient from POLYNOMIAL."
  (remhash monomial (polynomial-terms polynomial)))

(defun coeff (monomial polynomial)
  "Get coefficient associated with MONOMIAL in POLYNOMIAL. This returns 0 if
MONOMIAL does not explicitly appear in POLYNOMIAL."
  (or (gethash monomial (polynomial-terms polynomial)) 0))

(defun (setf coeff) (coefficient monomial polynomial)
  "Set coefficient associated with MONOMIAL in POLYNOMIAL to
COEFFICIENT. Does nothing if MONOMIAL is 0."
  (unless (eql monomial 0)
    (etypecase monomial
      (monomial (if (zerop coefficient)
                    (remove-monomial monomial polynomial)
                    (setf (gethash monomial (polynomial-terms polynomial))
                          coefficient))))))

(defun term (monomial polynomial)
  "Return the term associated with MONOMIAL in POLYNOMIAL. This is a new
polynomial containing just MONOMIAL multiplied by its coefficient in
POLYNOMIAL."
  (polynomial (coeff monomial polynomial) monomial))

(defun monomials (object)
  "Return a list of the monomials in OBJECT, which should be a polynomial or
hash table."
  (hash-table-keys (if (polynomial-p object)
                       (polynomial-terms object)
                       object)))

(defun monomials< (object)
  "Return a sorted list of the monomials in OBJECT (a polynomial or hash
table)."
  (sort (monomials object) #'monomial<))

(defmacro do-polynomial ((coefficient monomial polynomial &optional result)
                         &body body)
  "Iterate over the terms in POLYNOMIAL, with monomials bound to MONOMIAL and
corresponding coefficients bound to COEFFICIENT."
  `(dohash (,monomial ,coefficient (polynomial-terms ,polynomial) ,result)
     ,@body))

(defmacro do-polynomial-< ((coefficient monomial polynomial &optional result)
                           &body body)
  "Iterate over the terms in POLYNOMIAL in order of monomials, with monomials
bound to MONOMIAL and corresponding coefficients bound to COEFFICIENT."
  (once-only (polynomial)
    `(dolist (,monomial (monomials< ,polynomial) ,result)
       (let ((,coefficient (coeff ,monomial ,polynomial)))
         ,@body))))

(defun terms (polynomial)
  "Return the number of terms (monomials with nonzero coefficients) in
POLYNOMIAL."
  (hash-table-count (polynomial-terms polynomial)))

(defun zero-polynomial-p (polynomial)
  "Return true if POLYNOMIAL is zero."
  (zerop (terms polynomial)))

(defun max-monomial (polynomial)
  "Return highest monomial in POLYNOMIAL."
  (if (zero-polynomial-p polynomial)
      0
      (let ((max *id*))
        (do-polynomial (nil m polynomial)
          (when (monomial> m max)
            (setf max m)))
        max)))

(defgeneric conj (x)
  (:documentation "Compute and return the conjugate of X."))

(defmethod conj ((x number))
  (conjugate x))

(defmethod conj ((x monomial))
  (make-monomial (loop for (site . projectors) in (monomial-sites x)
                       collect (cons site (reverse projectors)))))

(defmethod conj ((x polynomial))
  (let ((result (polynomial)))
    (do-polynomial (c m x result)
      (setf (coeff (conj m) result) (conjugate c)))))

(defun conj-min (monomial)
  "Return either monomial X or its conjugate, whichever comes first
lexicographically."
  (monomial-min monomial (conj monomial)))

(defgeneric c* (x y)
  (:documentation "Multiplication of conjugate of X with Y."))

(defmethod c* ((x number) (y number))
  (* (conjugate x) y))

(defmethod c* ((x number) (y monomial))
  (polynomial (conjugate x) y))

(defmethod c* ((x monomial) (y number))
  (polynomial y (conj x)))

(defmethod c* ((x monomial) (y monomial))
  (monomial* x y #'site-c*
             #'reverse
             #'sites-conj))

(defmethod c* ((x number) (y polynomial))
  (let ((result (polynomial)))
    (do-polynomial (c m y result)
      (setf (coeff m result) (* (conjugate x) c)))))

(defmethod c* ((x polynomial) (y number))
  (let ((result (polynomial)))
    (do-polynomial (c m x result)
      (setf (coeff (conj m) result) (* (conjugate c) y)))))

(defmethod c* ((x monomial) (y polynomial))
  (let ((result (polynomial)))
    (do-polynomial (c m y result)
      (incf (coeff (c* x m) result) c))))

(defmethod c* ((x polynomial) (y monomial))
  (let ((result (polynomial)))
    (do-polynomial (c m x result)
      (incf (coeff (c* m y) result) (conjugate c)))))

(defmethod c* ((x polynomial) (y polynomial))
  (let ((result (polynomial)))
    (do-polynomial (cx mx x)
      (do-polynomial (cy my y)
        (incf (coeff (c* mx my) result)
              (* (conjugate cx) cy))))
    result))

(defgeneric 2arg* (x y)
  (:documentation "Multiplication of X and Y."))

(defmethod 2arg* ((x number) (y number))
  (* x y))

(defmethod 2arg* ((x number) (y monomial))
  (polynomial x y))

(defmethod 2arg* ((x monomial) (y number))
  (polynomial y x))

(defmethod 2arg* ((x monomial) (y monomial))
  (monomial* x y #'site* #'identity #'identity))

(defmethod 2arg* ((x number) (y polynomial))
  (let ((result (polynomial)))
    (do-polynomial (c m y result)
      (setf (coeff m result) (* x c)))))

(defmethod 2arg* ((x polynomial) (y number))
  (2arg* y x))

(defmethod 2arg* ((x monomial) (y polynomial))
  (let ((result (polynomial)))
    (do-polynomial (c m y result)
      (incf (coeff (2arg* x m) result) c))))

(defmethod 2arg* ((x polynomial) (y monomial))
  (let ((result (polynomial)))
    (do-polynomial (c m x result)
      (incf (coeff (2arg* m y) result) c))))

(defmethod 2arg* ((x polynomial) (y polynomial))
  (let ((result (polynomial)))
    (do-polynomial (cx mx x)
      (do-polynomial (cy my y)
        (incf (coeff (2arg* mx my) result) (* cx cy))))
    result))

(defun scale (polynomial number)
  "Scale (multiply the coefficients of) POLYNOMIAL by NUMBER and return the
result. Unless NUMBER is 0 or 1 this modifies POLYNOMIAL."
  (cond
    ((= number 0) (polynomial))
    ((= number 1) polynomial)
    (t (do-polynomial (c m polynomial polynomial)
         (setf (coeff m polynomial) (* number c))))))

(defun p* (&rest args)
  "Multiplication of any number of numbers, monomials, and polynomials."
  (if (null args)
      1
      (loop for result = (first args)
              then (2arg* result x)
            for x in (rest args)
            finally (return result))))

(defun increment (polynomial &rest args)
  "Increment POLYNOMIAL by remaining arguments (which can be numbers,
monomials, and polynomials) and return the result. This modifies POLYNOMIAL."
  (dolist (x args)
    (typecase x
      (number (incf (coeff *id* polynomial) x))
      (monomial (incf (coeff x polynomial)))
      (polynomial (do-polynomial (c m x)
                    (incf (coeff m polynomial) c)))))
  polynomial)

(defun decrement (polynomial &rest args)
  "Decrement POLYNOMIAL by remaining arguments (which can be numbers,
monomials, and polynomials) and return the result. This modifies POLYNOMIAL."
  (dolist (x args)
    (typecase x
      (number (decf (coeff *id* polynomial) x))
      (monomial (decf (coeff x polynomial)))
      (polynomial (do-polynomial (c m x)
                    (decf (coeff m polynomial) c)))))
  polynomial)

(defun p+ (&rest args)
  "Sum of any number of numbers, monomials, and polynomials."
  (let ((sum 0))
    (let (x)
      (loop
        (unless args (return-from p+ sum))
        (typecase (setf x (first args))
          (number
           (incf sum x)
           (setf args (rest args)))
          (t (return)))))
    (apply #'increment (polynomial) sum args)))

(defun p- (&rest args)
  "If there are no arguments, return 0. If there is only one argument,
return its negation. Otherwise return the first argument minus the sum of all
the other arguments."
  (if (null args)
      0
      (destructuring-bind (x . args) args
        (if (null args)
            (typecase x
              (number (- x))
              (monomial (polynomial -1 x))
              (polynomial (2arg* x -1)))
            (let ((sum (apply #'p+ args)))
              (if (numberp x)
                  (if (numberp sum)
                      (- x sum)
                      (increment (scale sum -1) x))
                  (if (numberp sum)
                      (decrement (fresh-polynomial x) sum)
                      (increment (scale sum -1) x))))))))

(defun pexpt (x n)
  "Raise X to the power of N. If X is a monomial or polynomial then N should
be a nonnegative integer."
  (cond
    ((numberp x) (expt x n))
    (t (check-type n (integer 0 *))
       (etypecase x
         (monomial (if (monomial= x *id*) *id* x))
         (polynomial (loop for p = (polynomial *id*) then (2arg* p x)
                           repeat n
                           finally (return p)))))))

(defmacro range-accum ((init fname) (var start-or-below &optional below)
                       &body expr)
  "Accumulate expression EXPR over the given range using function named FNAME
starting with initial value INIT."
  (assert (sequence-of-length-p expr 1))
  (setf expr (first expr))
  (let (start)
    (if below
        (setf start start-or-below)
        (setf start 0
              below start-or-below))
    (with-gensyms (result)
      (once-only (start below)
        `(let ((,result ,init))
           (do ((,var ,start (1+ ,var)))
               ((>= ,var ,below) ,result)
             (setf ,result (,fname ,result ,expr))))))))

(defmacro psum ((var start-or-below &optional below) &body expr)
  "Return the sum of repeated evaluations of EXPR with VAR bound to integers
in the given range. The range is from 0 below (not including) START-OR-BELOW
if the argument BELOW is omitted, otherwise the range is from START-OR-BELOW
up to and not including BELOW."
  `(range-accum ((polynomial) increment)
                (,var ,start-or-below ,below) ,@expr))

(defmacro pprod ((var start-or-below &optional below) &body expr)
  "Return the product of repeated evaluations of EXPR with VAR bound to
integers in the given range. The range is from 0 below (not including)
START-OR-BELOW if the argument BELOW is omitted, otherwise the range is from
START-OR-BELOW up to and not including BELOW."
  `(range-accum ((polynomial 1) p*) (,var ,start-or-below ,below) ,@expr))


(defun substitute-monomial (monomial polynomial constraint)
  "Attempt to remove MONOMIAL from POLYNOMIAL under the assumption that
CONSTRAINT is zero. POLYNOMIAL is modified unless there is nothing to
do (i.e., either POLYNOMIAL or CONSTRAINT don't contain MONOMIAL)."
  (let ((x (coeff monomial polynomial)))
    (if (zerop x)
        polynomial
        (progn
          (setf constraint (polynomial constraint))
          (let ((c (coeff monomial constraint)))
            (if (zerop c)
                polynomial
                (let ((x/c (/ x c)))
                  (progn
                    (remove-monomial monomial polynomial)
                    (do-polynomial (cp mp constraint)
                      (unless (equalp mp monomial)
                        (decf (coeff mp polynomial) (* cp x/c))))
                    polynomial))))))))

(defun diop (site &optional (input *default-input*))
  "Return dichotomic operator for given SITE and INPUT. This is two times the
projector with site number SITE, output *DEFAULT-OUTPUT*, and given INPUT
minus the identity."
  (decrement (polynomial 2 site input) *id*))

(defun set-add (monomial monomial-set)
  "Add MONOMIAL as a key in MONOMIAL-SET (a hash table). This is intended for
cases where you want to collect monomials as keys into a hash table and don't
care about associating them with hash values."
  (setf (gethash monomial monomial-set) nil))

(defun set-merge (monomial-set &rest more-sets)
  "Add monomials in MORE-SETS to MONOMIAL-SET and return it."
  (dolist (set more-sets)
    (dohash (monomial nil set)
      (set-add monomial monomial-set)))
  monomial-set)

(defun monomial-set (&rest initial-keys)
  "Create a set of monomials (a hash table with monomials as keys and values
set to nil)."
  (let ((table (make-monomial-hash-table)))
    (dolist (m initial-keys)
      (set-add m table))
    table))

(defun set* (&rest monomial-sets)
  "Return the product of MONOMIAL-SETS. This is the monomial set (hash table
with monomials as keys) containing all nonzero products of combinations of one
monomial from each of MONOMIAL-SETS."
  (let ((result (monomial-set)))
    (when monomial-sets
      (labels ((add-product (sets product)
                 (unless (eql product 0)
                   (if sets
                       (dohash (monomial nil (first sets))
                         (add-product (rest sets) (2arg* product monomial)))
                       (set-add product result)))))
        (dohash (monomial nil (first monomial-sets))
          (add-product (rest monomial-sets) monomial))))
    result))

(defparameter *alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Alphabet of characters to use in site labels.")

(defun ord-char (c)
  "Convert character to ordinal number, with the first character in
*ALPHABET* mapping to 0."
  (position c *alphabet*))

(defun char-ord (n)
  "Convert ordinal number to character, with 0 mapping to the first character
in *ALPHABET*."
  (char *alphabet* n))

(defun site->string (site)
  "Convert site number SITE to string representation using characters in
*ALPHABET*. Using the default alphabet this maps nonnegative numbers to the
letters A to Z, then AA to ZZ, then AAA to ZZZ, and so on."
  (let ((char-list ()))
    (loop with alpha-len = (length *alphabet*)
          for k = site then (- (floor k alpha-len) 1)
          for cn = (rem k alpha-len)
          while (>= k 0)
          do (push (char-ord cn) char-list))
    (with-output-to-string (s)
      (dolist (c char-list)
        (princ c s)))))

(defun string->site (string)
  "Convert STRING to site number, starting at the beginning of the string and
continuing until either the end of the string is reached or a character not
in *ALPHABET* is encountered. The first return value is the site number or
NIL if STRING does not start with a character in the alphabet. The second
return value is the rest of the string starting from the first character not
in the alphabet if one was found, otherwise NIL is returned."
  (loop with alpha-len = (length *alphabet*)
        with site = -1
        for i upfrom 0
        for c across string
        for k = (ord-char c)
        until (null k)
        do (setf site (+ k (* alpha-len (1+ site))))
        finally (return (values (if (>= site 0) site nil)
                                (if (< i (length string))
                                    (subseq string i)
                                    nil)))))

(defun string->natural (string)
  "Checks if STRING is strictly the string representation of a natural
number (i.e., STRING is at least one character long, it contains only the
characters 0-9 and nothing else, and the first character is not 0 unless it
is the only character) and returns that number if it is or NIL if not."
  (let ((len (length string)))
    (and (> len 0)
         (or (= len 1) (char/= (char string 0) #\0))
         (every #'digit-char-p string)
         (values (parse-integer string)))))

(defun write-site (site stream)
  "Print projectors at SITE."
  (let ((site-id (site->string (first site)))
        (projectors (rest site)))
    (flet ((print-projector (projector)
             (destructuring-bind (c . z) projector
               (write-string site-id stream)
               (princ c stream)
               (write-char #\| stream)
               (princ z stream))))
      (print-projector (first projectors))
      (dolist (p (rest projectors))
        (write-char #\Space stream)
        (pprint-newline :fill stream)
        (print-projector p)))))

(defun write-monomial (monomial stream)
  "Print a representation of MONOMIAL to STREAM."
  (with-slots (sites) monomial
    (cond
      ((eq sites ()) (write-string "Id" stream))
      (t (write-site (first sites) stream)
         (dolist (s (rest sites))
           (write-char #\Space stream)
           (pprint-newline :fill stream)
           (write-site s stream))))))

(defun monomial-string (monomial)
  "Return a string representation of MONOMIAL."
  (with-output-to-string (string)
    (write-monomial monomial string)))

(defun write-polynomial (polynomial stream)
  "Print a representation of POLYNOMIAL to STREAM."
  (let ((monomials (monomials< polynomial)))
    (if (null monomials)
        (princ 0 stream)
        (labels ((pos? (z)
                   (let ((x (realpart z)))
                     (or (> x 0)
                         (and (zerop x) (>= (imagpart z) 0)))))
                 (print-num (prefix z)
                   (write-string prefix stream)
                   (unless (eql z 1)
                     (princ z stream)
                     (write-char #\Space stream)))
                 (print-term (pos-prefix neg-prefix monomial)
                   (let ((c (coeff monomial polynomial)))
                     (if (pos? c)
                         (print-num pos-prefix c)
                         (print-num neg-prefix (- c)))
                     (write-monomial monomial stream))))
          (destructuring-bind (m . monomials) monomials
            (print-term "" "-" m)
            (dolist (m monomials)
              (write-char #\Space stream)
              (pprint-newline :fill stream)
              (print-term "+ " "- " m)))))))

(defun polynomial-string (polynomial)
  "Return a string representation of POLYNOMIAL."
  (with-output-to-string (string)
    (write-polynomial polynomial string)))

(defmacro operator-case ((symbol-or-string &optional site output input)
                         &body cases)
  "Convert SYMBOL-OR-STRING to a string if it isn't one already, then return
the result of evaluating one of the (up to four) CASES (an identity,
projector, dichotomic operator, and other case, in that order) depending on
the form of the string, with the site number, output, or input bound to SITE,
OUTPUT, or INPUT, as appropriate. The identity is the string \"ID\" in any
case. Projectors consist of one or more letters in *ALPHABET* indicating the
site, followed by an output number, followed by a '|' or '/', followed by an
input number, e.g., \"A1/1\". Dichotomic operators have the form of one or
more letters in *ALPHABET* followed by an input number, e.g., \"A1\". For
example,
  (OPERATOR-CASE (\"A1/2\" S C Z)
    \"Identity\"
    (FORMAT NIL \"Projector: ~d ~d ~d\" S C Z)
    (FORMAT NIL \"Dichotomic: ~d ~d\" S Z)
    \"Other\")
would return the string \"Projector: 0 1 2\"."
  (destructuring-bind (identity-case projector-case
                       dichotomic-case other-case . rest)
      cases
    (unless (endp rest) (error "More than four cases supplied."))
    (when (null site) (setf site (gensym "SITE")))
    (when (null output) (setf output (gensym "OUTPUT")))
    (when (null input) (setf input (gensym "INPUT")))
    (with-gensyms (string str c pos op-block)
      `(block ,op-block
         (let ((,string (string ,symbol-or-string)))
           (if (equalp ,string "ID")
               (return-from ,op-block ,identity-case)
               (multiple-value-bind (,site ,str) (string->site ,string)
                 (declare (ignorable ,site))
                 (if-let (,pos (position-if
                                (lambda (,c) (or (char= ,c #\/)
                                                 (char= ,c #\|)))
                                ,str))
                   (when-let (,output (string->natural (subseq ,str 0 ,pos)))
                     (when-let (,input (string->natural
                                        (subseq ,str (1+ ,pos))))
                       (return-from ,op-block ,projector-case)))
                   (when-let (,input (string->natural ,str))
                     (return-from ,op-block ,dichotomic-case))))))
         ,other-case))))

(defun operator (symbol-or-string)
  "Convert SYMBOL-OR-STRING to a string (if it isn't one already) and
construct the corresponding operator. If the string is \"ID\" (in any case)
then return the identity.  Otherwise if it starts with a sequence of
characters in *ALPHABET* this is converted to a site number. If the rest of
the string contains two natural numbers separated by a '|' or '/'
character (e.g. \"1|1\") then this is read as an output and input and the
corresponding projector is returned. If it contains a single natural number
then this is read as an input number and the corresponding dichotomic
operator (2 times the corresponding projector with output *DEFAULT-OUTPUT*
minus the identity, e.g. 2 A1|1 - Id) is returned. Otherwise NIL is
returned."
  (operator-case (symbol-or-string site output input)
    *id*
    (projector site output input)
    (diop site input)
    nil))

(defmacro with-operators ((&rest symbols) &body body)
  "Evaluate BODY with SYMBOLS bound to their corresponding operators. The
value of the last form in BODY is returned."
  `(let ,(loop for s in symbols collect `(,s (operator ',s)))
     ,@body))

(defun symbol-string= (x y)
  "Return T if X and Y are either symbols, strings, or characters and they
test equal as strings. Return NIL otherwise."
  (and (or (symbolp x) (stringp x) (characterp x))
       (or (symbolp y) (stringp y) (characterp y))
       (string= x y)))

(defun +or-p (obj)
  "Return T if OBJ is a symbol, string, or character and tests equal as a
string to either \"+\" or \"-\"."
  (or (symbol-string= obj "+") (symbol-string= obj "-")))

(defun +or-sep (list)
  "Returns two lists: a list of all the items in LIST following a '+' symbol,
string, or character, and a list of all the items in LIST following a '-'. If
LIST does not start with a '+' or '-' it is treated as if it started with a
'+'."
  (unless (+or-p (first list)) (push '+ list))
  (let (pos neg subexpr)
    (macrolet ((push-t-reset (obj place)
		 `(unless (null ,obj)
		    (push ,obj ,place)
		    (setf ,obj nil))))
      (dolist (obj (reverse list) (values pos neg))
	(cond
	  ((symbol-string= obj "+") (push-t-reset subexpr pos))
	  ((symbol-string= obj "-") (push-t-reset subexpr neg))
	  (t (push obj subexpr)))))))

(defun list-of-length-p (object n)
  "Return T if OBJECT is a list and its length is N. Return NIL otherwise."
  (and (listp object)
       (sequence-of-length-p object n)))

(declaim (ftype function expand-expr))

(defun expand+ (expr)
  "Translate additive expression EXPR from infix to prefix notation. For
example, (EXPAND+ '(1 + 2 - 3 - 4)) => (P- (P+ 1 2) 3 4). The subexpressions
E in EXPR are recursively expanded according to (EXPAND-EXPR E NIL)."
  (multiple-value-bind (pos neg) (+or-sep expr)
    (let ((pos (mapcar #'expand-expr pos))
	  (neg (mapcar #'expand-expr neg)))
      (flet ((+expr (exprs)
	       (if (list-of-length-p exprs 1)
		   (first exprs)
		   (cons 'p+ exprs))))
        (cond
          ((null neg) (+expr pos))
          ((null pos) `(p- (p+ ,@neg)))
          (t `(p- ,(+expr pos) ,@neg)))))))

(defun expand* (expr)
  "Expand EXPR treating it as a multiplicative expression. This expands the
subexpressions E in EXPR by calling (EXPAND-EXPR E T) then prepends P*."
   (cons 'p* (loop for e in expr collect (expand-expr e t))))

(defun funcall-p (expr)
  "Return T if EXPR looks like a function or operator call, i.e., if EXPR is
a list whose first element is a symbol for which FBOUNDP returns true."
  (and (listp expr)
       (let ((item (first expr)))
         (and (symbolp item) (fboundp item)))))

(defun expand-funcall (expr)
  "Expand EXPR as a function call. This returns EXPR with (EXPAND-EXPR E T)
called on all the items E in EXPR except for the first one."
  (cons (first expr) (loop for e in (rest expr)
                           collect (expand-expr e t))))

(defun expand-expr (expr &optional (expand-funcall nil))
  "Recursively translate EXPR from infix to prefix form."
  (cond
    ((atom expr) expr)
    ((member-if #'+or-p expr) (expand+ expr))
    ((and expand-funcall (funcall-p expr)) (expand-funcall expr))
    ((list-of-length-p expr 1) (expand-expr (first expr) t))
    (t (expand* expr))))

(defmacro infix (&rest expr)
  "Evaluate expression EXPR after translating from infix to prefix notation,
replacing +, -, and juxtaposed items with appropriate calls to the functions
P+, P- and P*. For example, evaluating (INFIX (A + B) (C - D)) is equivalent
to evaluating (P* (P+ A B) (P- C D))."
  (expand-expr expr nil))

(defun operator-code (symbol)
  "Test if SYMBOL represents an operator. Returns an expression
that will evaluate to the corresponding operator if it is, and NIL
otherwise."
  (operator-case (symbol site output input)
    '*id*
    `(projector ,site ,output ,input)
    `(diop ,site ,input)
    nil))

(defun operator-names-and-values (tree &optional (ignore ()))
  "Finds symbols in TREE that name an operator and returns a list containing
a list of each symbol found and an expression that will construct the
corresponding operator."
  (let ((pairs ()))
    (dotree (x tree pairs)
      (when-let (op (and (symbolp x)
                         (not (member x ignore :test #'eq))
                         (operator-code x)))
        (pushnew (list x op) pairs :test #'eq :key #'first)))))

(defmacro auto-operators (&body body)
  "Evaluate body with any symbols that name an operator found in the body
bound to the corresponding monomial or polynomial."
  `(let ,(operator-names-and-values body)
     ,@body))

(defmacro px (&rest expr)
  "Evaluate infix expression EXPR with any symbol in EXPR that names an
operator bound to the corresponding monomial or polynomial."
  `(let ,(operator-names-and-values expr)
     ,(expand-expr expr)))

(defun mod-range (n n0 range)
  "Return N modulo RANGE, except in such a way that the result is between N0
and N0 + RANGE instead of 0 and RANGE."
  (+ n0 (mod (- n n0) range)))

(defun pmod (site output input noutputs)
  "Return a projector for SITE and INPUT and an output modulo NOUTPUTS (which
must be at least 2). This determines an output based on OUTPUT between
*DEFAULT-OUTPUT* and *DEFAULT-OUTPUT* + NOUTPUTS (exclusive). The
corresponding projector is returned unless the output number is the maximum
possible (i.e., *DEFAULT-OUTPUT* + NOUTPUTS - 1), in which case the identity
minus the sum of all the preceding projectors is returned."
  (check-type noutputs (integer 2 *))
  (let ((c0 *default-output*))
    (let ((c (+ c0 (mod (- output c0) noutputs))))
      (if (< c (1- (+ c0 noutputs)))
          (projector site c input)
          (p- *id*
              (psum (k c0 c) (projector site k input)))))))

(defun input-mod (input ninputs)
  "Return an input number computed from INPUT in the range *DEFAULT-INPUT*
and *DEFAULT-INPUT* + NINPUTS - 1."
  (check-type ninputs (integer 1 *))
  (mod-range input *default-input* ninputs))

(defun dmod (site input ninputs)
  "Return the dichotomic operator (2 times the projector with output
*DEFAULT-OUTPUT* minus the identity) at site number SITE and with an input
number between *DEFAULT-INPUT* and *DEFAULT-INPUT* + NINPUTS - 1, which is
computed by modulo from INPUT."
  (diop site (input-mod input ninputs)))

(defun bk-prime (operator &optional (ninputs 2))
  "Return the \"prime\" transformation of OPERATOR as in the recursive
definition of the Bell-Klyshko operator. This is the same operator as
OPERATOR except all of the input numbers are increased by 1 modulo
NINPUTS (default 2). The input numbers in the result are in the range
*DEFAULT-INPUT* and *DEFAULT-INPUT* + NINPUTS - 1."
  (etypecase operator
    (number operator)
    (monomial
     (let ((result *id*))
       (do-sites (s c z operator)
         (setf result
               (2arg* result
                      (projector s c (input-mod (1+ z) ninputs)))))
       result))
    (polynomial
     (let ((result (polynomial (coeff *id* operator))))
       (do-polynomial (c m operator)
         (setf (coeff (bk-prime m ninputs) result) c))
       result))))

(defun chsh ()
  "Return the CHSH polynomial."
  (let ((i1 *default-input*)
        (i2 (1+ *default-input*)))
    (let ((b1 (diop 1 i1))
          (b2 (diop 1 i2)))
      (increment (p* (diop 0 i1) (p+ b1 b2))
                 (p* (diop 0 i2) (p- b1 b2))))))

(defun mermin (&optional (sites 3))
  "Return the Mermin polynomial for given number of SITES."
  (let ((i1 *default-input*)
        (i2 (1+ *default-input*)))
    (flet ((u (site)
             (increment (diop site i1)
                        (p* #c(0 1) (diop site i2)))))
      (let ((product (pprod (k sites) (u k))))
        (scale (increment product (conj product)) 1/2)))))

(defun klyshko (sites)
  "Return the Bell-Klyshko operator for given number of SITES."
  (check-type sites (integer 1 *))
  (let ((i1 *default-input*)
        (i2 (1+ *default-input*)))
    (labels ((bk (site)
               (if (= site 0)
                   (p- (diop 0 i1))
                   (let ((a1 (bk (1- site)))
                         (b1 (diop site i1))
                         (b2 (diop site i2)))
                     (scale (increment (p* a1 (p+ b1 b2))
                                       (p* (bk-prime a1) (p- b1 b2)))
                            1/2)))))
      (bk (1- sites)))))

(defun cglmp (&optional (outputs 3))
  "Return the CGLMP polynomial with number OUTPUTS of outputs."
  (check-type outputs (integer 2 *))
  (let ((d outputs))
    (flet ((p= (x y k)
             (psum (c d)
               (p* (pmod 0 c x d) (pmod 1 (+ c k) y d)))))
      (psum (k (floor d 2))
        (p* (- 1 (/ (* 2 k) (1- d)))
            (p- (p+ (p= 1 1 k) (p= 2 1 (- (1+ k)))
                    (p= 2 2 k) (p= 1 2 (- k)))
                (p+ (p= 1 1 (- (1+ k))) (p= 2 1 k)
                    (p= 2 2 (- (1+ k))) (p= 1 2 (1+ k)))))))))
