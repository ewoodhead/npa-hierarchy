(in-package :sdpa)

(defparameter *solver* "sdpa"
  "Name of the solver to use, e.g. \"sdpa\" or \"sdpa_gmp\".")

(defparameter *mode* 'default
  "Controls how SDPA is run. If it is one of the symbols DEFAULT, FAST, or
STABLE, SDPA is run in the corresponding mode via the -pt switch. If it is a
string or pathname it is considered the name of a parameter file. Otherwise
the parameter file is assumed to be a file named \"param.<*solver*>\" in
the current directory. In both cases SDPA is run with the indicated parameter
file, via the -p switch, if it exists, or with no mode option if it does not
exist.")

(defparameter *threads* 1
  "Number of threads used by SDPA.")

(defparameter *sdpa-float-type* 'double-float
  "Default floating-point type to use when writing and reading from SDPA
files.")

(defparameter *tmp-file-rootname* ".npa_sdpa_tmp"
  "Default name to use for SDPA files, not including the .dat-s and .out
extensions.")

(defparameter *delete-sdpa-tmp-files* t
  "Controls whether temporary SDPA files are deleted after SDPA has been
run interactively.")

(defclass sdp-problem ()
  ((costs
    :initarg :costs
    :type list)
   (constraints
    :initarg :constraints
    :type list)
   (maximise
    :initarg :maximise
    :initform nil
    :type boolean)))

(defun sdp-problem (costs constraints &optional (maximise nil))
  (make-instance 'sdp-problem
                 :costs costs
                 :constraints constraints
                 :maximise maximise))

(defun ratio->float (number)
  "Return an integer or floating-point representation of NUMBER. This returns
NUMBER converted to a floating-point number of type *SDPA-FLOAT-TYPE* if it
is a ratio."
  (check-type number real)
  (typecase number
    (integer number)
    (ratio (coerce number *sdpa-float-type*))
    (t number)))

(defun write-constraint-matrix (n constraint stream
                                &optional (key #'identity))
  "Write Nth constraint matrix to STREAM. Applies the function KEY to the
coefficient before printing it."
  (do-block-matrix (c block i j constraint)
    (format stream "~d ~d ~d ~d ~a~%" n block i j
            (funcall key (ratio->float c)))))

(defun analyse-problem (problem)
  "Returns the number of costs and blocks and the block structure in
PROBLEM. Signals an error if the number of costs and constraint matrices are
not equal."
  (with-slots (costs constraints) problem
    (let ((ncosts (length costs))
          (nconstraints (length constraints))
          (blockstruct (block-structure constraints)))
      (assert (= ncosts nconstraints))
      (values ncosts
              (length blockstruct)
              (coerce blockstruct 'list)))))

(defun export-problem (problem &optional (stream *standard-output*))
  "Write PROBLEM to STREAM in the sparse input format understood by SDPA."
  (with-slots (costs constraints maximise) problem
    (flet ((dsign (x) (if maximise (- x) x)))
      (format stream "*Offset = ~a~%" (dsign (first costs)))
      (format stream "*Maximise = ~a~%" (not (not maximise)))
      (multiple-value-bind (ncosts nblocks blockstruct)
          (analyse-problem problem)
        (format stream "  ~d = mDIM~%" (1- ncosts))
        (format stream "  ~d = nBLOCK~%" nblocks)
        (format stream "  (~{~d~^, ~}) = bLOCKsTRUCT~%" blockstruct))
      (flet ((sign-cost (c) (ratio->float (dsign c))))
        (format stream "~{~a~^ ~}~%" (mapcar #'sign-cost (rest costs)))))
    (write-constraint-matrix 0 (first constraints) stream #'-)
    (loop for constraint in (rest constraints)
          for n upfrom 1
          do (write-constraint-matrix n constraint stream))))

(defun export-to-file (filename problem &optional (deletep t))
  "Write PROBLEM to file named FILENAME in format expected by SDPA."
  (with-open-file (stream filename :direction :output
                          :if-exists (if deletep :supersede :error))
    (export-problem problem stream)))

(defun run-sdpa (in-file out-file &optional (output nil))
  "Run SDPA with input file IN-FILE and output file OUT-FILE."
  (let ((invocation (format nil "~a -ds ~a -o ~a"
                            *solver*
                            (merge-pathnames in-file)
                            (merge-pathnames out-file)))
        (mode (flet ((param-opt (path)
                       (if (probe-file path)
                           (format nil " -p ~a" path)
                           "")))
                (cond
                  ((eq *mode* 'default) " -pt 0")
                  ((eq *mode* 'fast) " -pt 1")
                  ((eq *mode* 'stable) " -pt 2")
                  ((typep *mode* 'pathname)
                   (param-opt *mode*))
                  ((typep *mode* 'string)
                   (param-opt (merge-pathnames *mode*)))
                  (t (param-opt (merge-pathnames
                                 (format nil "param.~a" *solver*)))))))
        (threads (if (typep *threads* '(integer 1 *))
                     (format nil " -numThreads ~a" *threads*)
                     "")))
    (run (format nil "~a~a~a" invocation mode threads) :output output)))

(defun search-forward (string stream)
  "Advance position in STREAM to just after the first instance of STRING."
  (let* ((len (length string))
	 (buffer (make-array len :element-type 'character)))
    (dotimes (i len)
      (setf (aref buffer i) (read-char stream)))
    (flet ((shift-append (c)
             (let ((end (1- len)))
               (dotimes (i end)
                 (setf (aref buffer i) (aref buffer (1+ i))))
               (setf (aref buffer end) c))))
        (loop until (string= string buffer)
              do (shift-append (read-char stream))))))

(defun get-item (label &optional (stream *standard-input*))
  "Read the object from STREAM following the string LABEL."
  (search-forward label stream)
  (search-forward "=" stream)
  (read stream))

(defun get-items (labels &optional (stream *standard-input*))
  "Return a list of the objects read from STREAM following the string labels
in the list LABELS."
  (loop for l in labels collect (get-item l stream)))

(defun extract-from-stream (&optional (stream *standard-input*))
  "Extract and return primal and dual solutions and status from SDPA output
read from STREAM."
  (let ((*read-default-float-format* *sdpa-float-type*))
    (destructuring-bind (offset maximise phase primal dual)
        (get-items '("*Offset" "*Maximise"
                     "phase.value" "objValPrimal" "objValDual")
                   stream)
      (flet ((adjust (x)
               (setf x (+ x offset))
               (if maximise (- x) x)))
          (values (adjust primal) (adjust dual) phase)))))

(defun extract-solution (&optional (filename-or-stream *standard-input*))
  "Extract and return primal and dual solutions and status from SDPA output
read from file named FILENAME."
  (if (streamp filename-or-stream)
      (extract-from-stream filename-or-stream)
      (with-open-file (s filename-or-stream :direction :input)
        (extract-solution s))))

(defun solve (problem &optional (fname *tmp-file-rootname*)
                        (delete-files *delete-sdpa-tmp-files*))
  "Solve PROBLEM. This consists of writing the specified problem as an SDPA
input file, running SDPA on it, and extracting from the output file and
returning the primal and dual solutions and a status indicator."
  (let ((in-file (make-pathname :name fname :type "dat-s"))
	(out-file (make-pathname :name fname :type "out")))
    (export-to-file in-file problem nil)
    (unwind-protect
         (progn
           (run-sdpa in-file out-file)
           (extract-solution out-file))
      (when delete-files
        (delete-file in-file)
        (delete-file out-file)))))
