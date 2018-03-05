;;; General purpose in-memory B+ tree.
;;; Copyright (c) 2012, Francisco Soto All rights reserved (see COPYING file for details).

;;; cl-bplustree.asd

(asdf:defsystem #:cl-bplustree
  :name "cl-bplustree"
  :author "Francisco Soto <ebobby@ebobby.org>"
  :license "BSD"
  :description "In-memory B+ tree"
  :components ((:file "packages")
               (:file "bplustree" :depends-on ("packages")))
  :in-order-to ((asdf:test-op
                 (asdf:test-op #:cl-bplustree-test))))

(asdf:defsystem #:cl-bplustree-test
  :name "cl-bplustree-test"
  :depends-on (#:cl-bplustree)
  :components
  ((:file "test"))
  :perform (asdf:test-op (op c)
              (funcall (intern #.(string '#:bplustree-test) :org.ebobby.bplustree))))
