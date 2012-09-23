;;; General purpose in-memory B+ tree.
;;; Copyright (c) 2012, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :org.ebobby.bplustree)

;; Builds a tree with random
(defun build-test-tree (order n &key (random-max 99999999999999999999))
  "Builds a tree with the given order and at most n elements with random records.
   Keys are numbers band the records are the same as the keys but coerced to strings.
   Also returns an a-list of the inserted key/records."
  (let ((items (loop
                  for i from 1 to n
                  for record = (write-to-string (random random-max))
                  do (print record)
                  collect record)))
    (values (apply #'bplustree-insert-many
                   (bplustree-new order
                                  :key (lambda (r) (parse-integer r))
                                  :comparer (lambda (n m) (cond ((< n m) 1) ((> n m) -1) (t 0))))
                   items)
            items)))

(defun test-bplustree-insert-search (order n)
  "Builds a tree of the given order with at most n random elements, then a test is run trying to see
   if every element insterted is searchable."
  (multiple-value-bind (tree items) (build-test-tree order n)
    (loop
       for record in items for key = record
       do (format t "Searching key ~a : " key)
         (if (= (bplustree-search key tree) record)
             (format t "found.~%")
             (progn
               (format t "not found.~%")
               (return))))))
