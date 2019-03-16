(defpackage :npa-utilities
  (:use :common-lisp)
  (:import-from :alexandria :with-gensyms)
  (:export :dohash :print-hash :dolist<=
           :collect-to-list :collect :collect-end
           :maptree :dotree))

(defpackage :operators
  (:use :common-lisp :npa-utilities)
  (:import-from :alexandria :with-gensyms :once-only :if-let :when-let
                :hash-table-keys :sequence-of-length-p)
  (:export :monomial :monomial-p :projector :probability :p
           :monomial-hash-table
           :*id* :*default-input* :*default-output*
           :order :monomial= :monomial/=
           :monomial< :monomial> :monomial<= :monomial>=
           :monomial-max :monomial-min
           :do-sites :do-projectors
           :polynomial :polynomial-p :copy-polynomial :fresh-polynomial
           :remove-monomial :coeff :term
           :monomials :monomials<
           :do-polynomial :do-polynomial-<
           :terms :zero-polynomial-p :max-monomial
           :conj :conj-min :c*
           :scale :increment :decrement :p* :p+ :p- :pexpt
           :psum :pprod
           :substitute-monomial :diop
           :set-add :set-merge :monomial-set :set*
           :*alphabet* :site->string :string->site
           :write-monomial :monomial-string
           :write-polynomial :polynomial-string
           :operator-names-and-values
           :operator :with-operators
           :expand-expr :infix :auto-operators :px
           :pmod :dmod :chsh :mermin :klyshko :cglmp))

(defpackage :block-matrix
  (:use :common-lisp)
  (:import-from :alexandria :with-gensyms :if-let :when-let :maxf
                :curry :assoc-value)
  (:export :block-matrix :map-block-matrix :do-block-matrix
           :mcoeff :mincrement :mscale :block-structure))

(defpackage :sdpa
  (:use :common-lisp :alexandria :inferior-shell :block-matrix)
  (:import-from :split-sequence :split-sequence)
  (:export :*solver* :*mode* :default :stable :fast
           :*threads* :*sdpa-float-type*
           :*tmp-file-rootname* :*overwrite-tmp-files*
           :*scale-ratio* :*comment-length* :*read-comments* :*read-xvec*
           :sdp-problem :costs :constraints :maximise :comments
           :sdp-costs :sdp-constraints :sdp-maximise :sdp-comments
           :offset :export-problem :export-to-file :run-sdpa
           :read-comments-from-stream :extract-comments
           :read-xvec-from-stream :extract-xvec
           :extract-solution :extract-from-file :solve))

(defpackage :npa-hierarchy
  (:use :common-lisp :operators :block-matrix :sdpa :npa-utilities)
  (:import-from :split-sequence :split-sequence :split-sequence-if)
  (:import-from :alexandria :hash-table-keys
                :if-let :when-let :assoc-value :maxf
                :sequence-of-length-p)
  (:export :*return-expectation-values*
           :translate-to-sdp :monomials-at-level
           :npa-moments :moments-at-npa-level
           :npa->sdp :optimise :minimise :maximise :problem
           :npa-solve :solve-problem
           :expectation-values :write-expectation-values
           :print-expectation-values))

(defpackage :npa-user
  (:use :common-lisp :operators :npa-hierarchy
        :block-matrix :npa-utilities :sdpa))
