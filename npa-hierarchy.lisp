(in-package :npa-hierarchy)

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

(defun substitute-constraint (constraint monomial moment-matrices)
  "Eliminate MONOMIAL from MOMENT-MATRICES (a hash table with monomials as
keys and block matrices as values) under the constraint that the polynomial
CONSTRAINT is zero. This modifies MOMENT-MATRICES (unless MONOMIAL is not in
CONSTRAINT) and returns the new hash table with MONOMIAL eliminated."
  (let ((y (coeff monomial constraint)))
    (unless (zerop y)
      (let ((matrix (gethash monomial moment-matrices)))
        (do-polynomial (x m (p- constraint (term monomial constraint)))
          (let ((scale (- (/ x y))))
            (mincrement (gethash m moment-matrices)
                        scale
                        matrix))))
      (remhash monomial moment-matrices)))
  moment-matrices)

(defun substitute-constraints (constraints objective moment-matrices)
  "Use CONSTRAINTS to eliminate monomials from OBJECTIVE and table of
MOMENT-MATRICES. Returns the modified OBJECTIVE polynomial and
MOMENT-MATRICES table. This function is destructive: all the arguments are
modified."
  (loop while constraints
        do (multiple-value-bind (mmax max-constraint remaining-constraints)
               (find-max-monomial constraints)
             (setf constraints remaining-constraints)
             (substitute-monomial mmax objective max-constraint)
             (substitute-constraint max-constraint mmax moment-matrices)
             (dolist (c constraints)
               (substitute-monomial mmax c max-constraint)))
        finally (return (values objective moment-matrices))))

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

(defun convert-to-sdp (objective constraints level maximise)
  "Convert NPA problem to semidefinite programming problem. This is the same
as the NPA->SDP function except that the arguments OBJECTIVE and CONSTRAINTS
are modified and must be a polynomial and list of polynomials, respectively."
  (merge-conjugates objective)
  (mapc #'merge-conjugates constraints)
  (let ((moment-matrices (moments-at-npa-level
                          (cons objective constraints)
                          level)))
    (substitute-constraints constraints objective moment-matrices)
    (multiple-value-bind (costs moments)
        (loop with monomials = (sort (hash-table-keys moment-matrices)
                                     #'monomial<)
              initially (assert (monomial= (first monomials) *id*))
              for mon in monomials
              for moment = (gethash mon moment-matrices)
              collect (coeff mon objective) into costs
              collect moment into moments
              finally (return (values costs moments)))
      (sdp-problem costs moments maximise))))

(defun npa->sdp (objective constraints level &optional (maximise nil))
  "Translate NPA problem (optimisation of OBJECTIVE subject to the list of
CONSTRAINTS being zero at level LEVEL of the NPA hierarchy) to a semidefinite
programming problem. The problem is taken to be a minimisation problem unless
the optional argument MAXIMISE is non-nil."
  (convert-to-sdp (fresh-polynomial objective)
                  (mapcar #'fresh-polynomial constraints)
                  level
                  maximise))

(defun optimise (objective constraints level &optional (maximise nil))
  "Maximise or minimise OBJECTIVE subject to the list of CONSTRAINTS at given
LEVEL of the NPA hierarchy."
  (solve (npa->sdp objective constraints level maximise)))

(defun maximise (objective level &rest constraints)
  "Maximise polynomial OBJECTIVE at given LEVEL of the NPA hierarchy subject
to zero or more CONSTRAINTS."
  (optimise objective constraints level t))

(defun minimise (objective level &rest constraints)
  "Minimise polynomial OBJECTIVE at given LEVEL of the NPA hierarchy subject
to zero or more CONSTRAINTS."
  (optimise objective constraints level nil))

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

(defun get-constraints (st-form)
  "Rearrange constraints so they are in the form polynomial = 0."
  (loop for expr in st-form
        append (destructuring-bind (term1 . terms)
                   (nreverse (split-sequence '= expr))
                 (loop for term2 in terms
                       collect `(p- ,(expand-expr term2)
                                    ,(expand-expr term1))))))

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
    (let ((constraints (get-constraints (subject-to-form forms)))
          (definitions (get-definitions (where-form forms)))
          (level (level-form forms)))
      (when (null level) (error "Hierarchy level missing"))
      `(let ,(operator-names-and-values
              (list objective constraints (mapcar #'rest definitions))
              (mapcar #'first definitions))
         (let* ,definitions
           (npa->sdp ,objective
                     (list ,@constraints)
                     ',level
                     ,maximise))))))

(defmacro solve-problem (&rest forms)
  `(solve (problem ,@forms)))