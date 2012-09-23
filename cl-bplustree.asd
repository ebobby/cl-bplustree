;;; General purpose in-memory B+ tree.
;;; Copyright (c) 2012, Francisco Soto All rights reserved (see COPYING file for details).

;;; cl-bplustree.asd

(defsystem #:cl-bplustree
    :name "cl-bplustree"
    :author "Francisco Soto <ebobby@ebobby.org>"
    :license "BSD"
    :description "In-memory B+ tree"
    :components ((:file "packages")
                 (:file "bplustree" :depends-on ("packages"))))
