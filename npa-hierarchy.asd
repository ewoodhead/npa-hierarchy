(defsystem "npa-hierarchy"
  :description
  "An implementation of the NPA hierarchy. Converts optimisation problems on
the quantum set expressed in the Collins-Gisin projection, for any number of
sites, inputs, and outputs, to semidefinite programming problem relaxations.
Works with the SDPA family of solvers."
  :author "Erik Woodhead <erik.woodhead@icfo.eu>"
  :depends-on ("alexandria" "split-sequence" "inferior-shell")
  :components ((:file "packages")
               (:file "npa-utilities" :depends-on ("packages"))
               (:file "operators"
                :depends-on ("packages" "npa-utilities"))
               (:file "block-matrix"
                :depends-on ("packages" "npa-utilities"))
               (:file "sdpa"
                :depends-on ("packages" "npa-utilities" "block-matrix"))
               (:file "npa-hierarchy"
                :depends-on ("packages" "npa-utilities" "operators"
                             "block-matrix" "sdpa"))))
