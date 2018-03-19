;;; General purpose in-memory B+ tree.
;;; Copyright (c) 2012, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :org.ebobby.bplustree)

(defstruct bplustree root depth order key comparer cache)
(defstruct node kind order size keys records prev-node next-node)

(defmethod print-object ((tree bplustree) str)
  (print-unreadable-object (tree str :identity t)
    (princ "B+ TREE" str)
    (princ ": " str)
    (princ (bplustree-depth tree) str)))

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

(defmacro with-comparer-function (tree-name &body body)
  "Wrap code inside a block that will create a dynamic variable that holds the comparer function."
  `(let ((*current-tree-comparer* (bplustree-comparer ,tree-name)))
     (declare (special *current-tree-comparer*))
     ,@body))

;; Build specialized functions to access the key and record internal collections.
(build-node-collection-accesors key)
(build-node-collection-accesors record)

;; Build specialized functions to transfer keys and records between nodes.
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
   :prev-node nil
   :next-node nil))

(defun search-node-keys (node key &key record-search)
  "Search the given node keys vector using binary search.
   Keys assumed to be sorted. Optional mix and max define the search space.
   The keyword record-search indicates if you are looking for a record or a node."
  (labels ((binary-search (min max)
             (declare (special *current-tree-comparer*))
             (if (< max min)
                 (unless record-search (1+ max))
                 (let* ((mid (+ min (ash (- max min) -1)))
                        (k (node-key node mid))
                        (cmp (funcall *current-tree-comparer* key k)))
                   (cond ((minusp cmp) (binary-search min (1- mid)))
                         ((plusp cmp) (binary-search (1+ mid) max))
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
  (let ((order (node-order node)))
    (cond ((< index order)
           (loop
              for i from index below (node-size node) do
                (node-key-transfer node node (1+ i) i)
                (node-record-transfer node node (1+ i) i)))
          ((eql index order)
           (node-key-set node (1- order) nil)
           (node-record-set node (1- order) nil)))))

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
                              (comparer (lambda (n m) (cond ((< n m) -1) ((> n m) 1) (t 0)))))
  "Makes a new B+ tree with the given order."
  (make-bplustree :root (build-node order :leaf)
                  :depth 1
                  :order order
                  :key key
                  :comparer comparer))

(defun bplustree-empty-p (tree)
  (let ((root (bplustree-root tree)))
    (unless (node-internal-p root)
      (zerop (node-size root)))))

(defun bplustree-search (key tree)
  "Search for a record in the given tree using the given key."
  (with-comparer-function tree
    (let ((cache (bplustree-cache tree)))
      (cond ((and cache
                  (zerop (funcall *current-tree-comparer* key (car cache))))
             (cadr cache))
            (t (let* ((leaf (find-leaf-node (bplustree-root tree) key))
                      (res (find-record leaf key)))
                 (when res
                   (setf (bplustree-cache tree) (list key res leaf)))
                 res))))))

(defun bplustree-search-range (from to tree)
  "Search and return a range of records in the given tree between the given keys."
  (with-comparer-function tree
    (loop
       with current-node = (find-leaf-node (bplustree-root tree) from)
       with initial-index = (search-node-keys current-node from)
       until (null current-node)
       appending
         (loop
            for i from initial-index below (node-num-keys current-node)
            for key = (node-key current-node i)
            for record = (node-record current-node i)
          while (<= (funcall *current-tree-comparer* key to) 0)
            collect record
            finally
              (setf current-node (node-next-node current-node))
              (setf initial-index 0)))))

(defun bplustree-search-next (key tree)
  "Return the first key in `tree` after the passed `key`.
Return the record as a second value.
If the third values is true, then the key and value were cached."
  (when (null tree)
    (return-from bplustree-search-next nil))
  (when (null key)
    (loop for node = (bplustree-root tree) then (node-record node 0)
       while (node-internal-p node)
       finally
         (return-from bplustree-search-next
           (unless (zerop (node-size node))
             (let ((key (node-key node 0))
                   (res (node-record node 0)))
               (setf (bplustree-cache tree) (list key res node))
               (values key res))))))
  (with-comparer-function tree
    (let ((cache (bplustree-cache tree))
          node index cached)
      (cond ((and cache (zerop (funcall *current-tree-comparer* key (car cache))))
             (setf cached t
                   node (third cache)
                   index (1- (search-node-keys node key))))
            (t (setf node (find-leaf-node (bplustree-root tree) key)
                     index (1- (search-node-keys node key)))))
      (when (or cached
                (>= index (node-size node))
                (< index 0)
                (zerop (funcall *current-tree-comparer*
                                key (node-key node index))))
        (incf index)
        (when (>= index (node-size node))
          (cond ((setf node (node-next-node node))
                 (setf index 0))
                (t (setf node nil)))))
      (when node
        (let ((key (node-key node index))
              (res (node-record node index)))
          (when key
            (setf (bplustree-cache tree) (list key res node))
            (values key res cached)))))))

(defun bplustree-search-prev (key tree)
  "Return the key in `tree` before the passed `key`.
Return the record as a second value.
If the third values is true, then the key and value were cached."
  (when (null tree)
    (return-from bplustree-search-prev nil))
  (when (null key)
    (loop for node = (bplustree-root tree)
       then (node-record node (1- (node-size node)))
       while (node-internal-p node)
       finally
         (return-from bplustree-search-prev
           (unless (zerop (node-size node))
             (let* ((index (1- (node-size node)))
                    (key (node-key node index))
                    (res (node-record node index)))
               (setf (bplustree-cache tree) (list key res node))
               (values key res))))))
  (with-comparer-function tree
    (let ((cache (bplustree-cache tree))
          node index cached)
      (cond ((and cache (zerop (funcall *current-tree-comparer* key (car cache))))
             (setf cached t
                   node (third cache)
                   index (1- (search-node-keys node key))))
            (t (setf node (find-leaf-node (bplustree-root tree) key)
                     index (1- (search-node-keys node key)))))
      (when (or cached
                (and (>= index 0)
                     (zerop (funcall *current-tree-comparer*
                                     key (node-key node index)))))
        (decf index))
      (when (< index 0)
        (cond ((setf node (node-prev-node node))
               (setf index (and node (1- (node-size node)))))
              (t (setf node nil))))
      (when node
        (let ((key (node-key node index))
              (res (node-record node index)))
          (when key
            (setf (bplustree-cache tree) (list key res node))
            (values key res cached)))))))

(defun bplustree-insert (record tree
                         &optional (key (funcall (bplustree-key tree) record)))
  "Insert a record into the given tree using the given key. Returns the tree with the new record inserted.
If `key` is included, uses that instead of calling the key function on `record`.
This enabled using the tree as a key/value store instead of a sorted set."
  (setf (bplustree-cache tree) nil)
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
                    (setf (node-prev-node new) node)
                    (let ((next (node-next-node node)))
                      (when next
                        (setf (node-prev-node next) new))
                      (setf (node-next-node new) next))
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
    (with-comparer-function tree
      (let ((new-node (insert-helper (bplustree-root tree) key record)))
        (when new-node
          (setf (bplustree-root tree) (build-new-root (bplustree-root tree) new-node))
          (incf (bplustree-depth tree)))
        tree))))

(defun bplustree-insert-many (tree &rest items)
  "Insert as many records given into the tree. Returns the tree with the new records inserted."
  (with-comparer-function tree
    (loop
       for record in items
       do (bplustree-insert record tree)
       finally (return tree))))

(defun bplustree-delete (key tree)
  "Deletes a record from the given tree using the given key. Returns the tree with the record deleted."
  (setf (bplustree-cache tree) nil)
  (labels ((balance-left (node child index)
             (when (> index 0)
               (let* ((left-child (node-record node (1- index)))
                      (left-size (node-size left-child)))
                 (when (> left-size (node-min-size left-child))
                   (move-records-right child 0)
                   (cond ((node-internal-p left-child)
                          (node-key-transfer node child (1- index) 0)
                          (node-key-transfer left-child node
                                             (- left-size 2) (1- index)
                                             :set-source-nil t))
                         (t (let ((key (node-key left-child (1- left-size))))
                              (node-key-set child 0 key)
                              (node-key-set node (1- index) key))))
                   (node-record-transfer left-child child (1- left-size) 0
                                         :set-source-nil t)
                   (incf (node-size child))
                   (decf (node-size left-child))
                   t))))
           (balance-right (node child index)
             (let ((size (node-size node)))
               (when (< index (- size 1))
                 (let* ((right-child (node-record node (1+ index)))
                        (right-size (node-size right-child))
                        (child-size (node-size child)))
                 (when (> right-size (node-min-size right-child))
                   (cond ((node-internal-p right-child)
                          (node-key-transfer node child index (1- child-size))
                          (node-key-transfer right-child node 0 index))
                         (t (node-key-transfer right-child child 0 child-size)
                            (node-key-transfer right-child node 1 index)))
                   (node-record-transfer right-child child 0 child-size)
                   (move-records-left right-child 0)
                   (incf (node-size child))
                   (decf (node-size right-child))
                   t)))))
           (merge-node (node child index)
             (let (left-child)
               (cond ((> index 0)
                      (setf left-child (node-record node (1- index))))
                     ((> (node-size node) 1)
                      (setf left-child child)
                      (incf index)
                      (setf child (node-record node index)))
                     (t (return-from merge-node)))
               (loop with child-size = (node-size child)
                  with left-size = (node-size left-child)
                  with internal-p = (node-internal-p left-child)
                  for i from 0 below child-size
                  for j from left-size
                  do
                    (node-key-transfer child left-child i j)
                    (node-record-transfer child left-child i j)
                  finally
                    (if internal-p
                      (node-key-transfer node left-child (1- index) (1- left-size)))
                    (node-key-transfer node node index (1- index))
                    (move-records-left node index)
                    (unless internal-p
                      (let ((next (node-next-node child)))
                        (when next
                          (setf (node-prev-node next) left-child))
                        (setf (node-next-node left-child) next)))
                    (incf (node-size left-child) child-size)
                    (decf (node-size node)))))
           (descend (node)
             (let* ((leaf-p (not (node-internal-p node)))
                    (index (search-node-keys node key :record-search leaf-p)))
               (cond (leaf-p
                      (when index
                        (move-records-left node index)
                        (decf (node-size node))
                        t))
                     (t (let* ((record (node-record node index)))
                          (when (descend record)
                            (when (node-underflow-p record)
                              (or (balance-left node record index)
                                  (balance-right node record index)
                                  (merge-node node record index))
                              t))))))))
    (with-comparer-function tree
      (let* ((root (bplustree-root tree)))
        (when (descend root)
          (when (and (node-internal-p root)
                     (eql 1 (node-size root)))
            (setf (bplustree-root tree) (node-record root 0))
            (decf (bplustree-depth tree))))
        tree))))

(defun bplustree-traverse-node (node fn)
  "Call `fn` on each leaf record in `node`."
  (cond ((null node))
        ((node-internal-p node)
         (loop for node across (node-records node)
            do (bplustree-traverse-node node fn)))
        (t (map nil
                (lambda (v)
                  (when v
                    (funcall fn v)))
                (node-records node)))))

(defun bplustree-traverse (tree fn)
  (bplustree-traverse-node (bplustree-root tree) fn))

(defun bplustree-traverse-node-with-keys (node fn)
  "Call `fn` with key and record for each leaf of `node`."
  (cond ((null node))
        ((node-internal-p node)
         (loop for node across (node-records node)
            do (bplustree-traverse-node-with-keys node fn)))
        (t (loop for i from 0 below (node-size node)
              for k = (node-key node i)
              for v = (node-record node i)
              do
                (funcall fn k v)))))

(defun bplustree-traverse-with-keys (tree fn)
  (bplustree-traverse-node-with-keys (bplustree-root tree) fn))

