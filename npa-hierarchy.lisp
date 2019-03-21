(in-package :npa-hierarchy)

(defparameter *return-expectation-values* nil
  "If non-null, return the expectation values of monomials by default.")

(defun projectors (operators &optional (sites ()))
  "Returns a set of all (non-identity) projectors found in the argument
OPERATORS (a monomial or polynomial or a list of operators). The second
return value is a hash table whose keys are the site numbers in SITES and
whose values are sets of the projectors found at the associated sites."
  (let ((table (monomial-set))
        (by-site (make-hash-table :size (length sites))))
    (dolist (site sites)
      (setf (gethash site by-site) (monomial-set)))
    (labels ((add-projectors (monomial)
               (do-sites (site c z monomial)
                 (let ((p (projector site c z)))
                   (set-add p table)
                   (when (member site sites)
                     (set-add p (gethash site by-site))))))
             (add-from-operator (operator)
               (typecase operator
                 (monomial (add-projectors operator))
                 (polynomial (do-polynomial (nil m operator)
                               (add-projectors m))))))
      (if (listp operators)
          (dolist (operator operators)
            (add-from-operator operator))
          (add-from-operator operators)))
    (values table by-site)))

(defun integer-level (projectors nlevel)
  "Return a set of the NLEVELth level monomials computed
from PROJECTORS."
  (let ((result (monomial-set *id*)))
    (loop with level = result
          repeat nlevel
          do (setf level (set* level projectors))
             (set-merge result level))
    result))

(defun extra-level (sites by-site)
  "Return the set of products of projectors found in BY-SITE for the site
numbers in the list SITES. BY-SITE should be a hash table with site numbers
as keys and corresponding sets of projectors at the sites as values."
  (flet ((projectors-at-site (site)
           (or (gethash site by-site)
               (error "No projectors for site ~a" (site->string site)))))
    (let ((result (projectors-at-site (first sites))))
      (dolist (site (rest sites))
        (setf result (set* result (projectors-at-site site))))
     result)))

(defun process-level (level)
  "Return 1) the base level (an integer) found in LEVEL, 2) a list of the
additional contributions that exceed the base level (with the sites converted
to integers), and 3) a list of all the sites seen in the additional
contributions. For example:

  (PROCESS-LEVEL '(1 + A B + A C + B C))
    => 1
       ((0 1) (0 2) (1 2))
       (0 1 2)

In this example, the second and third return values are non-empty only
because they indicate a contribution beyond the base level of 1.

If LEVEL is an integer then it is returned as the base (first return value)
and the two remaining return values are empty lists."
  (when (integerp level)
    (return-from process-level (values level () ())))
  (let ((base 0)
        (not-base ())
        (extra ())
        (sites ()))
    (dolist (term (split-sequence '+ level :test #'eq))
      (cond
        ((and (sequence-of-length-p term 1)
              (typep (first term) '(integer 0 *)))
         (maxf base (first term)))
        ((consp term)
         (push term not-base))
        ((null term) (error "Missing contribution in LEVEL"))
        (t (error "Unrecognised contribution ~s in LEVEL" term))))
    (dolist (term not-base)
      (unless (null (nthcdr base term))
        (let ((new-sites (sort (mapcar (lambda (x)
                                         (if (integerp x)
                                             x
                                             (string->site (string x))))
                                       term)
                               #'<)))
          (pushnew new-sites extra :test #'equal)
          (dolist (s new-sites)
            (pushnew s sites :test #'eql)))))
    (values base extra (sort sites #'<))))

(defun monomials-at-level (operators level)
  "Return the monomials made from the projectors in OPERATORS at given LEVEL
of the NPA hierarchy. LEVEL should be a nonnegative integer or a list
containing integers and symbols indicating sites separated by +
symbols, e.g., (1 + A B)."
  (multiple-value-bind (base extra sites) (process-level level)
    (multiple-value-bind (projectors by-site)
        (projectors operators sites)
      (let ((result (integer-level projectors base)))
        (dolist (sites extra)
          (set-merge result (extra-level sites by-site)))
        (monomials< result)))))

(defun npa-moments (monomials &optional (block-number 1)
                                (result (monomial-hash-table)))
  "Return a hash table of monomials (as keys) and associated block
matrices (as values). Each block matrix has coefficients equal to 1 in its
upper triangular part where the associated monomial appears in the NPA moment
matrix."
  (dolist<= ((mi i) (mj j)) (monomials 1)
    (let ((monomial (c* mi mj)))
      (unless (eql monomial 0)
        (setf monomial (conj-min monomial))
        (if-let (bmat (gethash monomial result))
          (setf (mcoeff bmat block-number i j) 1)
          (setf (gethash monomial result)
                (block-matrix block-number i j))))))
  result)

(defun moments-at-npa-level (operators level)
  "Return a hash table of monomials (as keys) and associated block
matrices (as values) for the monomials appearing in the NPA moment matrix at
the given LEVEL. The base projectors are extracted from OPERATORS, which can
be a monomial or polynomial or list of such operators."
  (npa-moments (monomials-at-level operators level)))

(defun find-max-monomial (polynomials)
  "Return 1) the maximum monomial in list of POLYNOMIALS, 2) the polynomial
it was found in, and 3) the original list POLYNOMIALS with the polynomial
from 2) removed. The list POLYNOMIALS is modified."
  (loop with maxm and maxp and max-cell and prev-cell
        for prev = nil then remaining
        for remaining on polynomials
        for p = (first remaining)
        for m = (max-monomial p)
        for biggest? = t then (monomial< maxm m)
        when biggest? do (setf maxm m
                               maxp p
                               max-cell remaining
                               prev-cell prev)
        finally (progn
                  (if (null prev-cell)
                      (setf polynomials (rest max-cell))
                      (setf (cdr prev-cell) (rest max-cell)))
                  (return (values maxm maxp polynomials)))))

(defun substitute-equality (equality monomial moment-matrices)
  "Eliminate MONOMIAL from MOMENT-MATRICES (a hash table with monomials as
keys and block matrices as values) under the constraint that the polynomial
EQUALITY is zero. This modifies MOMENT-MATRICES (unless MONOMIAL is not in
EQUALITY) and returns the new hash table with MONOMIAL eliminated."
  (let ((y (coeff monomial equality)))
    (unless (zerop y)
      (let ((matrix (gethash monomial moment-matrices)))
        (do-polynomial (x m (p- equality (term monomial equality)))
          (let ((scale (- (/ x y))))
            (mincrement (gethash m moment-matrices)
                        scale
                        matrix))))
      (remhash monomial moment-matrices)))
  moment-matrices)

(defun substitute-equalities (equalities inequalities
                              objective moment-matrices)
  "Use EQUALITIES to eliminate monomials from INEQUALITIES, OBJECTIVE, and
table of MOMENT-MATRICES. Returns the modified OBJECTIVE polynomial and
MOMENT-MATRICES table with INEQUALITIES merged in. This function is
destructive: all the arguments are modified."
  (flet ((remove-zeros ()
           (setf equalities (remove-if #'zero-polynomial-p equalities))))
    (remove-zeros)
    (loop while equalities
          do (multiple-value-bind (mmax max-equality remaining-equalities)
                 (find-max-monomial equalities)
               (setf equalities remaining-equalities)
               (when (monomial= mmax *Id*)
                 (error "Can't substitute ~s = 0.~%This probably means ~
                         there are contradictory equality constraints."
                        max-equality))
               (substitute-monomial mmax objective max-equality)
               (dolist (ineq inequalities)
                 (substitute-monomial mmax ineq max-equality))
               (substitute-equality max-equality mmax moment-matrices)
               (dolist (e equalities)
                 (substitute-monomial mmax e max-equality))
               (remove-zeros))))
  (loop for b upfrom 2
        for ineq in inequalities
        do (do-polynomial (c mon ineq)
             (setf (mcoeff (gethash mon moment-matrices) b 1 1) c)))
  (values objective moment-matrices))

(defun merge-conjugates (polynomial)
  "Merge terms in POLYNOMIAL whose monomials are adjoints of each other,
treating them as equal. The monomials left in POLYNOMIAL are those that come
lexicographically before their adjoints. For example, this function
transforms the polynomial A1/1 A1/2 + A1/2 A1/1 to 2 A1/1 A1/2. This function
is destructive: the argument POLYNOMIAL is modified."
  (do-polynomial (c m polynomial)
    (let ((m* (conj m)))
      (when (monomial< m* m)
        (remove-monomial m polynomial)
        (incf (coeff m* polynomial) c))))
  polynomial)

(defun convert-to-sdp (objective level equalities inequalities maximise)
  "Convert NPA problem to semidefinite programming problem. This is the same
as the NPA->SDP function except that the arguments OBJECTIVE and CONSTRAINTS
are modified and must be a polynomial and list of polynomials, respectively."
  (merge-conjugates objective)
  (mapc #'merge-conjugates equalities)
  (mapc #'merge-conjugates inequalities)
  (let ((moment-matrices (moments-at-npa-level
                          (append (cons objective equalities) inequalities)
                          level)))
    (substitute-equalities equalities inequalities objective moment-matrices)
    (multiple-value-bind (costs moments monomials)
        (loop with monomials = (sort (hash-table-keys moment-matrices)
                                     #'monomial<)
              initially (assert (monomial= (first monomials) *id*))
              for mon in monomials
              for moment = (gethash mon moment-matrices)
              collect (coeff mon objective) into costs
              collect moment into moments
              finally (return (values costs moments monomials)))
      (sdp-problem costs moments maximise
                   (list :monomials
                         (mapcar #'princ-to-string (rest monomials)))))))

(defun npa->sdp (objective level
                 &optional (equalities ()) (inequalities ()) (maximise nil))
  "Translate NPA problem (optimisation of OBJECTIVE at given LEVEL of the NPA
hierarchy, subject to the expectation values of the polynomials in the lists
EQUALITIES and INEQUALITIES being equal to and lower bounded by zero) to a
semidefinite programming problem. The problem is taken to be a minimisation
problem unless the optional argument MAXIMISE is non-nil."
  (convert-to-sdp (fresh-polynomial objective)
                  level
                  (mapcar #'fresh-polynomial equalities)
                  (mapcar #'fresh-polynomial inequalities)
                  maximise))

(defun optimise (objective level
                 &optional (equalities ()) (inequalities ()) (maximise nil))
  "Maximise or minimise OBJECTIVE subject to the list of EQUALITIES and
INEQUALITIES at given LEVEL of the NPA hierarchy."
  (npa-solve (npa->sdp objective level equalities inequalities maximise)))

(defun maximise (objective level &optional (equalities ()) (inequalities ()))
  "Maximise polynomial OBJECTIVE at given LEVEL of the NPA hierarchy subject
to zero or more EQUALITIES and INEQUALITIES."
  (optimise objective level equalities inequalities t))

(defun minimise (objective level &optional (equalities ()) (inequalities ()))
  "Minimise polynomial OBJECTIVE at given LEVEL of the NPA hierarchy subject
to zero or more EQUALITIES and INEQUALITIES."
  (optimise objective level equalities inequalities nil))

(defun find-form (forms &rest keys)
  (dolist (k keys)
    (when-let (form-value (assoc-value forms k :test #'string=))
      (return-from find-form form-value)))
  nil)

(defun minimise-form (forms)
  (find-form forms "MINIMISE" "MINIMIZE"))

(defun maximise-form (forms)
  (find-form forms "MAXIMISE" "MAXIMIZE"))

(defun subject-to-form (forms)
  (find-form forms "SUBJECT-TO"))

(defun where-form (forms)
  (find-form forms "WHERE" "WITH"))

(defun level-form (forms)
  (find-form forms "LEVEL"))

(defun get-objective (forms)
  (if-let (objective (maximise-form forms))
    (values (expand-expr objective) t)
    (values (expand-expr (minimise-form forms)) nil)))

(defun split-on (delimiters list)
  "Split LIST on DELIMITERS. The second return value is a list of delimiters
in DELIMITERS in the order they are found in LIST."
  (let ((delims ()))
    (values
     (split-sequence-if (lambda (x)
                          (let ((result (member x delimiters)))
                            (when result
                              (push x delims))
                            result))
                        list)
     (nreverse delims))))

(defun get-constraints (st-form)
  "Return equalities and inequalities rearranged in the form polynomial = 0
or polynomial >= 0."
  (let ((equalities ())
        (inequalities ()))
    (dolist (form st-form)
      (multiple-value-bind (exprs relations) (split-on '(= >= <= > <) form)
        (loop for x = (expand-expr (first exprs)) then y
              for expr in (rest exprs)
              for y = (expand-expr expr)
              for r in relations
              do (case r
                   (= (push `(p- ,x ,y) equalities))
                   ((>= >) (push `(p- ,x ,y) inequalities))
                   ((<= <) (push `(p- ,y ,x) inequalities))))))
    (values equalities inequalities)))

(defun get-definitions (where-form)
  "Return bindings in reverse order from WHERE-FORM."
  (let ((bindings ()))
    (dolist (assignment where-form)
      (destructuring-bind (var symb . value) assignment
        (unless (string= symb "=")
          (error "Missing `=` symbol"))
        (push (list var (expand-expr value)) bindings)))
    bindings))

(defmacro problem (&rest forms)
  (multiple-value-bind (objective maximise) (get-objective forms)
    (when (null objective) (error "Objective missing"))    
    (multiple-value-bind (equalities inequalities)
         (get-constraints (subject-to-form forms))
      (let ((definitions (get-definitions (where-form forms)))
            (level (level-form forms)))
        (when (null level) (error "Hierarchy level missing"))
        `(let ,(operator-names-and-values
                (list objective equalities (mapcar #'rest definitions))
                (mapcar #'first definitions))
           (let* ,definitions
             (npa->sdp ,objective
                       ',level
                       (list ,@equalities)
                       (list ,@inequalities)
                       ,maximise)))))))

(defun monomials->alist (monomials values)
  (mapcar (lambda (m x)
            (cons (reduce #'p* (mapcar #'operator
                                       (split-sequence #\Space m)))
                  x))
          monomials values))

(defun npa-solve (problem &optional (name nil))
  (let ((*read-comments* *return-expectation-values*)
        (*read-xvec* *return-expectation-values*))
    (if *return-expectation-values*
        (multiple-value-bind (primal dual phase xvec comments)
            (solve problem)
          (values primal dual phase
                  (monomials->alist (getf comments :monomials) xvec)))
        (if name
            (solve problem name)
            (solve problem)))))

(defun get-name (forms)
  (first (find-form forms "NAME")))

(defmacro solve-problem (&rest forms)
  (if-let (name (get-name forms))
    `(npa-solve (problem ,@forms) ,(if (eq name t)
                                       *tmp-file-rootname*
                                       name))
    `(npa-solve (problem ,@forms))))

(defun expectation-values (&optional (source (format nil "~a.out"
                                                     *tmp-file-rootname*)))
  "Return an alist of mononials and their expectation values from SOURCE."
  (flet ((read-exp-vals (stream)
           (monomials->alist
            (getf (read-comments-from-stream stream) :monomials)
            (read-xvec-from-stream stream))))
    (etypecase source
      (list source)
      (stream (read-exp-vals source))
      ((or pathname string) (with-open-file (s source :direction :input)
                              (read-exp-vals s))))))

(defun write-expectation-values (&optional (destination *standard-output*)
                                   (source (format nil "~a.out"
                                                   *tmp-file-rootname*)))
  "Print expectation values of monomials obtained from SOURCE to
DESTINATION."
  (flet ((write-exps (stream)
           (dolist (pair (expectation-values source))
             (destructuring-bind (m . x) pair
               (format stream "~@<<~a>~_ = ~a~:>~%" m x)))))
    (when (eq destination t)
      (setf destination *terminal-io*))
    (etypecase destination
      (stream (write-exps destination))
      ((or pathname string) (with-open-file (s destination :direction :output
                                               :if-exists :supersede)
                              (write-exps s)))
      (null (with-output-to-string (s)
              (write-exps s)))))
  (values))

(defun print-expectation-values (&optional
                                   (source (format nil "~a.out"
                                                   *tmp-file-rootname*)))
  "Print expectation values of monomials obtained from SOURCE to standard
output."
  (write-expectation-values *standard-output* source))
