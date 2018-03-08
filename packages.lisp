;;; General purpose in-memory B+ tree.
;;; Copyright (c) 2012, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :cl-user)

(defpackage :org.ebobby.bplustree
  (:use :common-lisp)
  (:export
   :bplustree-new
   :bplustree-empty-p
   :bplustree-search
   :bplustree-search-range
   :bplustree-search-next
   :bplustree-search-prev
   :bplustree-insert
   :bplustree-insert-many
   :bplustree-delete
   :bplustree-traverse
   :bplustree-traverse-with-keys))
