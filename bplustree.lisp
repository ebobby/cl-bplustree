;;; General purpose in-memory B+ tree.
;;;

;;; General access functions

; Our node type.
(defstruct bplustree-node
  kind
  order
  size
  keys
  records
  next-node)

; Builder of getter functions based on the setters given by the node struct.
(defmacro build-bplustree-node-setter (column)
  "Generates the setter functions for the btreeplus-node structure."
  (let* ((package (symbol-package column))
         (struct-getter (intern (concatenate 'string (string 'bplustree-node-) (string column)) package))
         (setter (intern (concatenate 'string (string struct-getter) (string '-set)) package)))
    `(defun ,setter (node value)
       (setf (,struct-getter node) value))))

; Builder of getter/getter functions of individual items in internal node collections.
(defmacro build-bplustree-node-collection-accesors (column)
  "Generates the getter/setter functions for the btreeplus-node internal collections, keys and records."
  (let* ((package (symbol-package column))
         (getter (intern (concatenate 'string (string 'bplustree-node-) (string column)) package))
         (setter (intern (concatenate 'string (string getter) (string '-set)) package))
         (base-collection (intern (concatenate 'string (string getter) (string 's)) (symbol-package column))))
    `(progn
       (defun ,getter (node i) (aref (,base-collection node) i))
       (defun ,setter (node i value) (setf (aref (,base-collection node) i) value)))))

; Build setter functions, analogous to the getters already provided by defstruct.
(build-bplustree-node-setter kind)
(build-bplustree-node-setter order)
(build-bplustree-node-setter size)
(build-bplustree-node-setter next-node)

; Build specialized functions to access the key and record internal collections.
(build-bplustree-node-collection-accesors key)
(build-bplustree-node-collection-accesors record)

(defun bplustree-node-internal-p (node)
  "Is the node an internal node?"
  (eq :internal (bplustree-node-kind node)))

(defun bplustree-node-leaf-p (node)
  "Is the node a leaf?"
  (eq :leaf (bplustree-node-kind node)))

(defun bplustree-node-illegal-p (node)
  "Does the node have more records than it should?"
  (> (bplustree-node-size node)
     (bplustree-node-order node)))

(defun bplustree-node-key-record-set (node n key record)
  "Sets both the key and record at the given index  to the given B+ node."
  (bplustree-node-key-set node n key)
  (bplustree-node-record-set node n record))

(defun bplustree-node-num-keys (node)
  "Get the number of keys based on the node size and node type."
  (- (bplustree-node-size node)
     (if (bplustree-node-internal-p node) 1 0)))

;;; Internal tree operations

(defun make-node (order &optional (kind :internal))
  "Makes an empty B+ tree node with the given order and the optional type (:leaf or :internal)."
  (make-bplustree-node
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
                        (k (bplustree-node-key node mid)))
                   (cond ((< key k) (binary-search min (1- mid)))
                         ((> key k) (binary-search (1+ mid) max))
                         (t (+ mid (if record-search 0 1))))))))
    (binary-search 0 (1- (bplustree-node-num-keys node)))))

(defun find-record (node key)
  "Get the record with the given key in the given node, nil if none."
  (let ((index (search-node-keys node key :record-search t)))
    (unless (null index)
      (bplustree-node-record node index))))

(defun find-node (node key)
  "Get the next node using the given key in the given node."
  (bplustree-node-record node (search-node-keys node key)))

(defun move-records (node index)
  "Move the keys and records from the given starting point to the right."
  (let ((max (bplustree-node-size node)))
    (loop for i from max downto index for j = (1- i) while (> i 0) do
         (bplustree-node-key-set node i (bplustree-node-key node j))
         (bplustree-node-record-set node i (bplustree-node-record node j)))
    (bplustree-node-key-record-set node index nil nil)))

(defun split-node (node)
  "Creates a new node and copies the upper half of the key/records in node,
   returning the new node."
  (loop
     with new = (make-node (bplustree-node-order node) (bplustree-node-kind node))
     with mid = (ash (bplustree-node-size node) -1)
     with size = (1- (bplustree-node-size node))
     with node-adjust = (if (bplustree-node-internal-p node) -1 0)
     for i from mid to size
     for j = 0 then (1+ j) do
       (bplustree-node-key-record-set new j (bplustree-node-key node (+ i node-adjust)) (bplustree-node-record node i))
       (bplustree-node-size-set new (1+ (bplustree-node-size new)))
       (bplustree-node-key-set node (+ i node-adjust) nil)
       (bplustree-node-record-set node i nil)
       (bplustree-node-size-set node (1- (bplustree-node-size node)))
     finally
       (bplustree-node-next-node-set node (when (bplustree-node-leaf-p node) new))
       (return new)))

(defun promote-first-key (node)
  "Promotes the first key in the node, if its a leaf it simply returns it, if its an internal
   node it returns it but shifts the other keys to the left."
  (let ((key (bplustree-node-key node 0))
        (num-keys (bplustree-node-num-keys node)))
    (if (bplustree-node-leaf-p node)
        key
        (loop for i from 0 to (1- num-keys) do
             (bplustree-node-key-set node i (bplustree-node-key node (1+ i)))
           finally (bplustree-node-key-set node num-keys nil)  (return key)))))

;;; Public interface

(defun bplustree-new (order)
  "Makes a new B+ tree with the given order."
  (make-node order :leaf))

(defun bplustree-search (key tree)
  "Search for a record in the given tree using the given key."
  (if (bplustree-node-internal-p tree)
      (bplustree-search key (find-node tree key))
      (find-record tree key)))

(defun bplustree-insert (key record tree)
  "Insert a record into the given tree using the given key.
   Returns the tree with the new record inserted. This call may destroy the given tree."
  (labels ((add-record (node key record)
             (let ((index (search-node-keys node key)))
               (move-records node (search-node-keys node key))
               (bplustree-node-key-record-set node index key record)
               (bplustree-node-size-set node (1+ (bplustree-node-size node)))))
           (add-key (node new-node)
             (let* ((new-key (promote-first-key new-node))
                    (index (search-node-keys node new-key)))
               (move-records node index)
               (bplustree-node-key-record-set node index new-key (bplustree-node-record node (1+ index)))
               (bplustree-node-record-set node (1+ index) new-node)
               (bplustree-node-size-set node (1+ (bplustree-node-size node)))))
           (build-new-root (old-root new-node)
             (let ((new-root (make-node (bplustree-node-order old-root))))
               (bplustree-node-key-set new-root 0 (promote-first-key new-node))
               (bplustree-node-record-set new-root 0 old-root)
               (bplustree-node-record-set new-root 1 new-node)
               (bplustree-node-size-set new-root 2)
               new-root))
           (insert-helper (node key record)
             (if (bplustree-node-internal-p node)
                 (let ((new-node (insert-helper (find-node node key) key record))) ; Traverse down the tree.
                   (when new-node                                                  ; Did we have a split?
                     (add-key node new-node)                                       ; Insert new node into its parent.
                     (when (bplustree-node-illegal-p node)                         ; Is this node larger than it should?
                       (split-node node))))                                        ; Split it.
                 (let ((update (search-node-keys node key :record-search t)))      ; Is this an update?
                   (cond (update (bplustree-node-key-record-set node update key record) nil)
                         (t (add-record node key record)                           ; Add record.
                            (when (bplustree-node-illegal-p node)                  ; Illegal leaf?
                              (split-node node))))))))                             ; Split it and return new node.
    (let ((new-node (insert-helper tree key record)))
      (if new-node
          (build-new-root tree new-node)                                           ; Have a new node? Build a new root.
          tree))))

(defun bplustree-insert-many (tree &rest items)
  "Insert as many pairs of key/record given in the form of (key record) into the tree.
   Returns the tree with the new records inserted. This call may destroy the given tree."
  (loop
     for (key record) in items
     do (setf tree (bplustree-insert key record tree))
     finally (return tree)))

;;; Testing code disregard

; Simple tree to test
(defun fake-tree ()
  "Create a b+ tree by manually for testing."
  (apply #'bplustree-insert-many (bplustree-new 4)
         '((4 "4") (1 "1") (3 "3") (17 "17") (30 "30") (5 "5"))))

; Used to run simple tests.
(defun runtest ()
   (bplustree-insert-many (fake-tree)
                          '(6 "6")
                          '(-1 "-1")
                          '(2 "2")
                          '(50 "50")
                          '(-20 "-20")
                          '(-10 "-10")
                          '(142 "142")
                          '(7 "7")))
