;;; General purpose in-memory B+ tree.
;;; Copyright (c) 2012, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :org.ebobby.bplustree)

(export 'bplustree-test)

;;
;; You can this with default parameters with:
;;
;;   (asdf:test-system :cl-bplustree)
;;
;; If successful, it will print:
;;
;;   B+ tree test successful.
;;
(defun bplustree-test (&key (count 1000) (order 4) (percent 70)
                         verbose-p check-p break-p)
  "Do a torture test.
`count` is the maximum number of keys inserted, default 1000.
`order` is the tree order, default 4.
`percent` is the percent of insertions in the first pass and deletions in the second.
`verbose-p`, `check-p`, and `break-p` all default to NIL.
If `verbose-p` is true, will print each insertion or deletion as it does them.
If `check-p` is true, will check the tree after each insertion or deletion,
  and error if it finds a problem.
If `check-p` is false, does only two checks, one after the insert pass and
  one after deleting all the keys at the end.
If `check-p` is false and `break-p` is true, will error if the check after the
  insert pass fails.
Prints an error or success message.
Returns three values:
   1) The tree, empty on success.
   2) The total number of insertions.
   3) The total number of deletions."
  (let* ((out (loop for i from 1 to count collect i))
         (out-count count)
         (in nil)
         (in-count 0)
         (add-count nil)
         (delete-count nil)
         (add-total 0)
         (delete-total 0)
         (loop-count (* count 10))
         (tree (bplustree-new order)))
    (labels ((do-loop (percent)
               (loop for i from 0 below loop-count
                  for in-p = (or add-count
                                 (unless delete-count
                                   (and out (<= (random 100) percent))))
                  for l = (if in-p out in)
                  for len = (if in-p out-count in-count)
                  for key = (and l (nth (random len) l))
                  do
                    (when (and (null in) (< percent 50))
                      (setf add-count (floor count 2)))
                    (when (and (null out) (>= percent 50))
                      (setf delete-count (floor count 2)))
                    (when add-count
                      (when (zerop (decf add-count))
                        (setf add-count nil)))
                    (when delete-count
                      (when (zerop (decf delete-count))
                        (setf delete-count nil)))
                    (if in-p
                        (incf add-total)
                        (incf delete-total))
                    (when key
                      (cond (in-p
                             (when verbose-p
                               (format t "+~d" key))
                             (bplustree-insert key tree)
                             (setf out (delete key out))
                             (push key in)
                             (decf out-count)
                             (incf in-count))
                            (t
                             (when verbose-p
                               (format t "-~d" key))
                             (bplustree-delete key tree)
                             (setf in (delete key in))
                             (push key out)
                             (decf in-count)
                             (incf out-count)))
                      (when check-p (check)))))
             (check (&optional (error-p t))
               (setf in (sort in #'<))
               (let (msg)
                 (unless (equal in (bplustree-search-range 0 count tree))
                   (setf msg "range mismatch")
                   (if error-p
                       (error msg)
                       (return-from check msg)))
                 (dolist (key in)
                   (unless (eql key (bplustree-search key tree))
                     (setf msg (format nil "key missing: ~s" key))
                     (if error-p
                         (error msg)
                         (return-from check msg))))
                 (dolist (key out)
                   (let ((val (bplustree-search key tree)))
                     (when val
                       (setf msg "Unexpected key/value: ~s/~s" key val)
                       (if error-p
                           (error msg)
                           (return-from check msg))))))))
      (let (msg)
        (block nil
          (do-loop percent)
          (unless check-p
            (setf msg (check nil))
            (when msg
              (setf msg (format nil "After add pass: ~a" msg))
              (if break-p
                  (error msg)
                  (return))))
          (when verbose-p (terpri))
          (do-loop (- 100 percent))
          (when verbose-p (terpri))
          (dolist (key in)
            (bplustree-delete key tree))
          (setf in nil)
          (let* ((depth (bplustree-depth tree))
                 (root (bplustree-root tree))
                 (size (node-size root)))
            (cond ((setf msg (check nil))
                   (setf msg (format nil "After delete: ~a" msg)))
                  ((not (eql 1 depth))
                   (setf msg
                         (format nil "Depth mismatch, should be: 1, was: ~d" depth)))
                  ((not (eql 0 size))
                   (setf msg
                         (format nil "Size mismatch, should be: 0, was: ~d" size)))
                  ((node-internal-p root)
                   (setf msg "Root of empty tree not a leaf node")))))
        (format t "~a~%" (or msg "B+ tree test successful."))
        (values tree add-total delete-total)))))
