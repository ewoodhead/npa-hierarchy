(in-package :sdpa)

(defparameter *solver* #-windows "sdpa" #+windows "SDPA"
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

(defparameter *overwrite-tmp-files* nil
  "Controls whether it is OK to write the SDPA input file if the default
temporary filename is being used and a file with that name already exists.")

(defparameter *scale-ratio* t
  "Control whether problems with rational coefficients are scaled to make
them integers.")

(defparameter *comment-length* 77
  "Maximum length of a comment line in the SDPA input file, not including the
'*' at the beginning.")

(defparameter *read-comments* nil
  "If non-null, read comments from SDPA output file and return them by
default.")

(defparameter *read-xvec* nil
  "If non-null, read xVec values from SDPA output file and return them by
default.")

(defclass sdp-problem ()
  ((costs
    :initarg :costs
    :type list
    :reader sdp-costs)
   (constraints
    :initarg :constraints
    :type list
    :reader sdp-constraints)
   (maximise
    :initarg :maximise
    :initform nil
    :type boolean
    :reader sdp-maximise)
   (comments
    :initarg :comments
    :initform nil
    :type (or null string)
    :reader sdp-comments)))

(defun sdp-problem (costs constraints
                    &optional (maximise nil) (comments nil))
  (make-instance 'sdp-problem
                 :costs costs
                 :constraints constraints
                 :maximise maximise
                 :comments comments))

(defun ratio->float (number)
  "Return an integer or floating-point representation of NUMBER. This returns
NUMBER converted to a floating-point number of type *SDPA-FLOAT-TYPE* if it
is a ratio."
  (etypecase number
    (integer number)
    (ratio (coerce number *sdpa-float-type*))
    (real number)))

(defun num->str (number)
  "Return an integer or floating-point string representation of NUMBER."
  (setf number (ratio->float number))
  (format nil
          (etypecase number
            (integer "~d")
            (real "~f"))
          number))

(defun end-line (stream)
  #+windows (princ #\Return stream)
  (terpri stream))

(defun write-constraint-matrix (n constraint stream scales
                                &optional (key #'identity) (base-block 1))
  "Write Nth constraint matrix to STREAM. Applies the function KEY to the
coefficient before printing it."
  (do-block-matrix (c block i j constraint)
    (format stream "~d ~d ~d ~d ~a" n block i j
            (num->str (funcall key
                               (* c (aref scales (- block base-block))))))
    (end-line stream)))

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

(defun get-scale (costs)
  (let ((scale 1))
    (dolist (c costs)
      (when (typep c 'ratio)
        (setf scale (lcm scale (denominator c)))))
    scale))

(defun get-scales (constraints scales &optional (base-block 1))
  (dolist (block-matrix constraints)
    (do-block-matrix (c b row col block-matrix)
      (when (typep c 'ratio)
        (let ((i (- b base-block)))
          (setf (aref scales i)
               (lcm (aref scales i) (denominator c)))))))
  scales)

(defun write-comments (comments &optional (stream *standard-output*))
  (let ((*print-right-margin* *comment-length*))
    (dolist (comment comments)
      (dolist (line (split-sequence #\Newline (prin1-to-string comment)))
        (write-char #\* stream)
        (write-string line stream)
        (end-line stream)))))

(defun export-problem (problem &optional (stream *standard-output*))
  "Write PROBLEM to STREAM in the sparse input format understood by SDPA."
  (with-slots (costs constraints maximise comments) problem
    (multiple-value-bind (ncosts nblocks blockstruct)
        (analyse-problem problem)
      (let ((scale 1)
            (scales (make-array nblocks :element-type '(integer 1 *)
                                :initial-element 1)))
        (when *scale-ratio*
          (setf scale (get-scale (rest costs))
                scales (get-scales constraints scales)))
        (setf maximise (not (not maximise)))
        (flet ((dsign (x) (if maximise (- x) x))
               (formatln (control-string &rest format-arguments)
                 (apply #'format stream control-string format-arguments)
                 (end-line stream)))
          (formatln "*Solution = ~a."
                    (if maximise
                        "-(SDP_sol / Scale + Offset)"
                        "SDP_sol / Scale + Offset"))
          (formatln "*Offset = ~a" (dsign (first costs)))
          (formatln "*Scale = ~d" scale)
          (formatln "*Maximise = ~a" maximise)
          (write-comments comments stream)
          (formatln "  ~d = mDIM" (1- ncosts))
          (formatln "  ~d = nBLOCK" nblocks)
          (formatln "  (~{~d~^, ~}) = bLOCKsTRUCT" blockstruct)
          (flet ((sign-cost (c) (num->str (dsign (* c scale)))))
            (formatln "~{~a~^ ~}" (mapcar #'sign-cost (rest costs)))))
        (write-constraint-matrix
         0 (first constraints) stream scales (lambda (c) (- c)))
        (loop for constraint in (rest constraints)
              for n upfrom 1
              do (write-constraint-matrix
                  n constraint stream scales))))))

(defun export-to-file (filename problem &optional (overwrite t))
  "Write PROBLEM to file named FILENAME in format expected by SDPA."
  (with-open-file (stream filename :direction :output
                          :if-exists (if overwrite :supersede :error))
    (export-problem problem stream)))

(defun run-sdpa (in-file out-file &optional (output nil) (overwrite t))
  "Run SDPA with input file IN-FILE and output file OUT-FILE."
  (setf in-file (merge-pathnames in-file)
        out-file (merge-pathnames out-file))
  (unless (or overwrite (not (probe-file out-file)))
    (error "Output file ~s already exists." out-file))
  (let ((invocation (format nil "~a -ds ~a -o ~a"
                            *solver* in-file out-file))
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
    (run (format nil "~a~a~a" invocation mode threads) :output output)
    (values)))

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

(defun read-comments (stream)
  (with-input-from-string
      (s (format nil "~{~a~^ ~}"
                 (loop for c = (read-char stream)
                       while (char= c #\*)
                       collect (read-line stream)
                       finally (unread-char c stream))))
    (loop with eof = '#:eof
          for object = (read s nil eof)
          until (eq object eof)
          collect object)))

(defun read-comments-from-stream (stream)
  (search-forward "*Maximise" stream)
  (read-line stream)
  (read-comments stream))

(defun extract-comments (&optional (source (format nil "~a.out"
                                                   *tmp-file-rootname*)))
  "Extract additional objects in comment lines from SOURCE. SOURCE should be
either a file name or input stream."
  (etypecase source
    (stream (read-comments-from-stream source))
    ((or pathname string) (with-open-file (s source :direction :input)
                           (read-comments-from-stream s)))))

(defun read-xvec-from-stream (stream)
  (search-forward "xVec" stream)
  (search-forward "{" stream)
  (mapcar #'read-from-string
          (split-sequence #\, (string-right-trim "} " (read-line stream)))))

(defun extract-xvec (&optional (source
                                (format nil "~a.out" *tmp-file-rootname*)))
  "Read xVec from SOURCE. SOURCE should be a file name or input stream."
  (etypecase source
    (stream (read-xvec-from-stream source))
    ((or pathname string) (with-open-file (s source :direction :input)
                            (read-xvec-from-stream s)))))

(defun extract-from-stream (&optional (stream *standard-input*)
                              (read-xvec *read-xvec*)
                              (read-comments *read-comments*))
  "Extract and return primal and dual solutions and status from SDPA output
read from STREAM."
  (let ((*read-default-float-format* *sdpa-float-type*))
    (destructuring-bind (offset scale maximise)
        (get-items '("*Offset" "*Scale" "*Maximise") stream)
      (setf offset (rational offset))
      (let ((xvec ())
            (comments ()))
        (when read-comments
          (setf comments (read-comments stream)))
        (destructuring-bind (phase primal dual)
           (get-items '("phase.value" "objValPrimal" "objValDual") stream)
         (flet ((adjust (x)
                  (setf x (coerce (+ (/ (rational x) scale) offset)
                                  'double-float))
                  (if maximise (- x) x)))
           (setf primal (adjust primal)
                 dual (adjust dual))
           (when read-xvec
             (setf xvec (read-xvec-from-stream stream)))
           (cond
             (read-comments (values primal dual phase xvec comments))
             (read-xvec (values primal dual phase xvec))
             (t (values primal dual phase)))))))))

(defun extract-solution (&optional (filename-or-stream *standard-input*)
                           (read-xvec *read-xvec*)
                           (read-comments *read-comments*))
  "Extract and return primal and dual solutions and status from SDPA output
read from file named FILENAME."
  (if (streamp filename-or-stream)
      (extract-from-stream filename-or-stream read-xvec read-comments)
      (with-open-file (s filename-or-stream :direction :input)
        (extract-from-stream s read-xvec read-comments))))

(defun solve (problem &optional (fname *tmp-file-rootname* fname-supplied-p)
                        (overwrite (or *overwrite-tmp-files*
                                       fname-supplied-p))
                        (delete-files (not (or *overwrite-tmp-files*
                                               fname-supplied-p))))
  "Solve PROBLEM. This consists of writing the specified problem as an SDPA
input file, running SDPA on it, and extracting from the output file and
returning the primal and dual solutions and a status indicator."
  (let ((in-file (make-pathname :name fname :type "dat-s"))
	(out-file (make-pathname :name fname :type "out")))
    (export-to-file in-file problem overwrite)
    (unwind-protect
         (progn
           (run-sdpa in-file out-file nil overwrite)
           (extract-solution out-file))
      (when delete-files
        (delete-file in-file)
        (delete-file out-file)))))
