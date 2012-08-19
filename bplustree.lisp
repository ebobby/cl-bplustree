;;; General purpose in-memory B+ tree.
;;; Copyright (c) 2012, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :bplustree)

(defstruct bplustree root depth order key comparer)
(defstruct node kind order size keys records next-node)

(defmacro build-node-collection-accesors (column)
  "Generates the getter/setter functions for the btreeplus-node internal collections, keys and records."
  (let* ((package (symbol-package column))
         (getter (intern (concatenate 'string (string 'node-) (string column)) package))
         (setter (intern (concatenate 'string (string getter) (string '-set)) package))
         (base-collection (intern (concatenate 'string (string getter) (string 's)) (symbol-package column))))
    `(progn
       (defun ,getter (node i) (aref (,base-collection node) i))
       (defun ,setter (node i value) (setf (aref (,base-collection node) i) value)))))

(defmacro build-node-collection-transfer (column)
  "Generates functions to transfer elements from a node into another node."
  (let* ((package (symbol-package column))
         (getter (intern (concatenate 'string (string 'node-) (string column)) package))
         (setter (intern (concatenate 'string (string getter) (string '-set)) package))
         (fname (intern (concatenate 'string (string getter) (string '-transfer)) package)))
    `(defun ,fname (source destination i-source i-destination &key set-source-nil)
       (,setter destination i-destination (,getter source i-source))
       (when set-source-nil (,setter source i-source nil)))))

; Build specialized functions to access the key and record internal collections.
(build-node-collection-accesors key)
(build-node-collection-accesors record)

; Build specialized functions to transfer keys and records between nodes.
(build-node-collection-transfer key)
(build-node-collection-transfer record)

(defun node-internal-p (node)
  "Is the node an internal node?"
  (eq :internal (node-kind node)))

(defun node-overflow-p (node)
  "Does the node have more records than it should?"
   (> (node-size node) (node-order node)))

(defun node-underflow-p (node)
  "Does the node have less records than it should?"
  (< (node-size node) (node-min-size node)))

(defun node-key-record-set (node n key record)
  "Sets both the key and record at the given index  to the given B+ node."
  (node-key-set node n key)
  (node-record-set node n record))

(defun node-num-keys (node)
  "Get the number of keys based on the node size and node type."
  (- (node-size node)
     (if (node-internal-p node) 1 0)))

(defun node-min-size (node)
  "Returns the minimum size a node can have (except root)."
  (ceiling (/ (node-order node) 2)))

(defun build-node (order &optional (kind :internal))
  "Makes an empty B+ tree node with the given order and the optional type (:leaf or :internal)."
  (make-node
   :order order
   :size 0
   :kind kind
   :keys (make-array (1+ order) :initial-element nil)
   :records (make-array (1+ order) :initial-element nil)
   :next-node nil))

(defun search-node-keys (node key &key record-search)
  "Search the given node keys vector using binary search.
   Keys assumed to be sorted. Optional mix and max define the search space.
   The keyword record-search indicates if you are looking for a record or a node."
  (labels ((binary-search (min max)
             (if (< max min)
                 (unless record-search (1+ max))
                 (let* ((mid (+ min (ash (- max min) -1)))
                        (k (node-key node mid)))
                   (cond ((< key k) (binary-search min (1- mid)))
                         ((> key k) (binary-search (1+ mid) max))
                         (t (+ mid (if record-search 0 1))))))))
    (binary-search 0 (1- (node-num-keys node)))))

(defun find-record (node key)
  "Get the record with the given key in the given node, nil if none."
  (let ((index (search-node-keys node key :record-search t)))
    (unless (null index)
      (node-record node index))))

(defun find-node (node key)
  "Get the next node using the given key in the given node."
  (node-record node (search-node-keys node key)))

(defun find-leaf-node (node key)
  "Find the proper leaf node for the given key."
  (if (node-internal-p node)
      (find-leaf-node (find-node node key) key)
      node))

(defun move-records-right (node index)
  "Move the keys and records from the given starting point to the right."
  (loop
     for i from (node-size node) downto index while (> i 0) do
       (node-key-transfer node node (1- i) i)
       (node-record-transfer node node (1- i) i)))

(defun move-records-left (node index)
  "Move the keys and records going left to right from given starting point."
  (loop
     for i from index below (node-size node) do
       (node-key-transfer node node (1+ i) i)
       (node-record-transfer node node (1+ i) i)))

(defun promote-first-key (node &key no-shift)
  "Promotes the first key in the node, if its a leaf it simply returns it, if its an internal
   node it returns it but shifts the other keys to the left."
  (let ((key (node-key node 0))
        (num-keys (node-num-keys node)))
    (if (or (not (node-internal-p node)) no-shift)
        key
        (loop
           for i from 0 below num-keys
           do (node-key-transfer node node (1+ i) i)
           finally
             (node-key-set node num-keys nil)
             (return key)))))

;;; Public interface

(defun bplustree-new (order &key
                              (key #'identity)
                              (comparer (lambda (n m)
                                          (cond ((< n m) -1)
                                                ((> n m) 1)
                                                (t 0)))))
  "Makes a new B+ tree with the given order."
  (make-bplustree :root (build-node order :leaf)
                  :depth 1
                  :order order
                  :key key
                  :comparer comparer))

(defun bplustree-search (key tree)
  "Search for a record in the given tree using the given key."
  (find-record (find-leaf-node (bplustree-root tree) key) key))

(defun bplustree-search-range (from to tree)
  "Search and return a range of records in the given tree between from and to inclusive."
  (loop
     with current-node = (find-leaf-node (bplustree-root tree) from)
     with initial-index = (search-node-keys current-node from)
     until (null current-node)
     appending
       (loop
          for i from initial-index below (node-num-keys current-node)
          for key = (node-key current-node i)
          for record = (node-record current-node i)
          while (<= key to)
          collect (list key record)
          finally
            (setf current-node (node-next-node current-node))
            (setf initial-index 0))))

(defun bplustree-insert (key record tree)
  "Insert a record into the given tree using the given key. Returns the tree with the new record inserted."
  (labels ((add-record (node key record)
             (let ((index (search-node-keys node key)))
               (move-records-right node index)
               (node-key-record-set node index key record)
               (incf (node-size node))))
           (add-key (node new-node)
             (let* ((new-key (promote-first-key new-node))
                    (index (search-node-keys node new-key)))
               (move-records-right node index)
               (node-key-set node index new-key)
               (node-record-transfer node node (1+ index) index)
               (node-record-set node (1+ index) new-node)
               (incf (node-size node))))
           (build-new-root (old-root new-node)
             (let ((new-root (build-node (node-order old-root))))
               (node-key-set new-root 0 (promote-first-key new-node))
               (node-record-set new-root 0 old-root)
               (node-record-set new-root 1 new-node)
               (setf (node-size new-root) 2)
               new-root))
           (split-node (node)
             (loop
                with new = (build-node (node-order node) (node-kind node))
                with node-adjust = (if (node-internal-p node) -1 0)
                for i from (node-min-size node) below (node-size node)
                for j = 0 then (1+ j) do
                  (node-key-transfer node new (+ i node-adjust) j :set-source-nil t)
                  (node-record-transfer node new i j :set-source-nil t)
                  (incf (node-size new))
                  (decf (node-size node))
                finally
                  (unless (node-internal-p node)
                    (setf (node-next-node new) (node-next-node node))
                    (setf (node-next-node node) new))
                  (return new)))
           (insert-helper (node key record)
             (if (node-internal-p node)
                 (let ((new-node (insert-helper (find-node node key) key record))) ; Traverse down the tree.
                   (when new-node                                                  ; Did we have a split?
                     (add-key node new-node)                                       ; Insert new node into its parent.
                     (when (node-overflow-p node)                                  ; Is this node larger than it should?
                       (split-node node))))                                        ; Split it.
                 (let ((update (search-node-keys node key :record-search t)))      ; Is this an update?
                   (cond (update (node-key-record-set node update key record) nil)
                         (t (add-record node key record)                           ; Add record.
                            (when (node-overflow-p node)                           ; Illegal leaf?
                              (split-node node))))))))                             ; Split it and return new node.
    (let ((new-node (insert-helper (bplustree-root tree) key record)))
      (when new-node
        (setf (bplustree-root tree) (build-new-root (bplustree-root tree) new-node))
        (incf (bplustree-depth tree)))
      tree)))

(defun bplustree-insert-many (tree &rest items)
  "Insert as many pairs of key/record given in the form of (key record) into the tree.
   Returns the tree with the new records inserted."
  (loop
     for (key record) in items
     do (bplustree-insert key record tree)
     finally (return tree)))

(defun bplustree-delete (key tree)
  "Deletes a record from the given tree using the given key. Returns the tree with the record deleted."
  (labels ((balance-node (node index)
             (cond ((and (plusp index)     ; Transfer record from the left side node.
                         (> (node-size (node-record node (1- index)))
                            (node-min-size (node-record node (1- index)))))
                    (let* ((l-node (node-record node (1- index)))
                           (r-node (node-record node index))
                           (l-node-key-i (1- (node-num-keys l-node)))
                           (l-node-record-i (1- (node-size l-node))))
                      (move-records-right r-node 0)
                      (node-key-transfer l-node r-node l-node-key-i 0 :set-source-nil t)
                      (node-record-transfer l-node r-node l-node-record-i 0 :set-source-nil t)
                      (when (node-internal-p r-node)
                        (node-key-set r-node 0 (promote-first-key (node-record r-node 1) :no-shift t)))
                      (node-key-set node (1- index) (promote-first-key r-node :no-shift t))
                      (decf (node-size l-node))
                      (incf (node-size r-node))))
                   ((and (< index (1- (node-size node))) ; Transfer record from the right side node.
                         (> (node-size (node-record node (1+ index)))
                            (node-min-size (node-record node (1+ index)))))
                    (let* ((l-node (node-record node index))
                           (r-node (node-record node (1+ index)))
                           (l-node-key-i (node-num-keys l-node))
                           (l-node-record-i (node-size l-node)))
                      (node-key-transfer r-node l-node 0 l-node-key-i)
                      (node-record-transfer r-node l-node 0 l-node-record-i)
                      (move-records-left r-node 0)
                      (when (node-internal-p l-node)
                        (node-key-set l-node l-node-key-i (promote-first-key (node-record l-node l-node-record-i) :no-shift t)))
                      (node-key-set node index (promote-first-key r-node :no-shift t))
                      (incf (node-size l-node))
                      (decf (node-size r-node))))
                   (t nil)))
           (merge-node (node index)
             (loop
                with l-node = (node-record node (- index (if (plusp index) 1 0)))
                with r-node = (node-record node (+ index (if (plusp index) 0 1)))
                for j from 0 below (node-size r-node)
                for i = (node-size l-node) then (1+ i)
                do
                  (node-key-transfer r-node l-node j i)
                  (node-record-transfer r-node l-node j i)
                  (incf (node-size l-node))
                finally
                  (move-records-left node (if (zerop index) 1 index))
                  (setf (node-next-node l-node) (node-next-node r-node))
                  (decf (node-size node))))
           (delete-helper (key node)
             (if (node-internal-p node)
                 (let* ((index (search-node-keys node key))
                        (child-node (node-record node index)))
                   (delete-helper key child-node)
                   (when (node-underflow-p child-node)
                     (unless (balance-node node index)
                       (merge-node node index))))
                 (let ((index (search-node-keys node key :record-search t)))
                   (when index
                     (move-records-left node index)
                     (decf (node-size node)))))))
    (let ((root (bplustree-root tree)))
      (delete-helper key root)
      (when (and (= (node-size root) 1) (node-internal-p root))
        (setf (bplustree-root tree) (node-record root 0))
        (decf (bplustree-depth tree)))
      tree)))
