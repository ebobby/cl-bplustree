;;; General purpose in-memory B+ tree.
;;;

;;; General access functions

(defun get-node-type (node)
  "Get the B+ tree node type."
  (elt node 0))

(defun set-node-type (node type)
  "Set the B+ tree node type."
  (setf (elt node 0) type))

(defun get-node-order (node)
  "Get the B+ tree node order."
  (elt node 1))

(defun get-node-size (node)
  "Get the B+ tree node order."
  (elt node 2))

(defun set-node-size (node size)
  "Set the B+ tree node order."
  (setf (elt node 2) size))

(defun get-node-keys (node)
  "Get the node keys vector."
  (elt node 3))

(defun get-node-records (node)
  "Get the node records vector."
  (elt node 4))

(defun get-node-next-node (node)
  "Get the next node following the linked list."
  (elt node 5))

(defun set-node-next-node (node next-node)
  "Set the next node in the linked list."
  (setf (elt node 5) next-node))

(defun get-node-key (node n)
  "Get the key at the given index from the given B+ tree node."
  (aref (get-node-keys node) n))

(defun set-node-key (node n key)
  "Set the key at the given index to the given B+ tree node."
  (setf (aref (get-node-keys node) n) key))

(defun get-node-record (node n)
  "Get the record at the given index from the given B+ tree node."
  (aref (get-node-records node) n))

(defun set-node-record (node n record)
  "Set the record at the given index to the given B+ tree node."
  (setf (aref (get-node-records node) n) record))

(defun is-node-p (node)
  "Is the node an internal node?"
  (eq :node (get-node-type node)))

(defun is-leaf-p (node)
  "Is the node a leaf?"
  (eq :leaf (get-node-type node)))

(defun is-node-full-p (node)
  "Is the node full? (Leaf nodes are full at order - 1)."
  (>= (get-node-size node)
      (- (get-node-order node) (if (is-leaf-p node) 1 0))))

(defun is-node-illegal-p (node)
  "Does the node have more records than it should?"
  (> (get-node-size node)
     (get-node-order node)))

(defun set-node-key-record (node n key record)
  "Sets both the key and record at the given index  to the given B+ node."
  (set-node-key node n key)
  (set-node-record node n record))

(defun get-node-num-keys (node)
  "Get the number of keys based on the node size and node type."
  (- (get-node-size node)
     (if (is-node-p node) 1 0)))

;;; Internal tree operations

(defun make-node (order &optional (type :node))
  "Makes an empty B+ tree node with the given order and the optional type (:leaf or :node)."
  (list type                                           ; Node type (:leaf or :node)
        order                                          ; Order
        0                                              ; Size
        (make-array (1+ order) :initial-element nil)   ; Keys
        (make-array (1+ order) :initial-element nil)   ; Nodes
        nil))                                          ; Next node (leaves only)

(defun search-node-keys (node key &key record-search)
  "Search the given node keys vector using binary search.
   Keys assumed to be sorted. Optional mix and max define the search space.
   The keyword record-search indicates if you are looking for a record or a node."
  (labels ((binary-search (min max)
             (if (< max min)
                 (unless record-search (1+ max))
                 (let* ((mid (+ min (ash (- max min) -1)))
                        (k (get-node-key node mid)))
                   (cond ((< key k) (binary-search min (1- mid)))
                         ((> key k) (binary-search (1+ mid) max))
                         (t (+ mid (if record-search 0 1))))))))
    (binary-search 0 (1- (get-node-num-keys node)))))

(defun find-record (node key)
  "Get the record with the given key in the given node, nil if none."
  (let ((index (search-node-keys node key :record-search t)))
    (unless (null index)
      (get-node-record node index))))

(defun find-node (node key)
  "Get the next node using the given key in the given node."
  (get-node-record node (search-node-keys node key)))

(defun move-records (node index)
  "Move the keys and records from the given starting point to the right."
  (let ((max (get-node-size node)))
    (loop for i from max downto index for j = (1- i) while (> i 0) do
         (set-node-key node i (get-node-key node j)))
    (loop for i from (+ max (if (is-node-p node) 1 0))
       downto index for j = (1- i) while (> i 0) do
         (set-node-record node i (get-node-record node j)))
    (set-node-key-record node index nil nil)))

(defun split-node (node)
  "Creates a new node and copies the upper half of the key/records in node,
   returning the new node."
  (loop
     with new = (make-node (get-node-order node) (get-node-type node))
     with mid = (ash (get-node-size node) -1)
     with size = (1- (get-node-size node))
     for i from mid to size
     for j = 0 then (1+ j) do
       (set-node-key-record new j (get-node-key node i) (get-node-record node i))
       (set-node-size new (1+ (get-node-size new)))
       (set-node-key-record node i nil nil)
       (set-node-size node (1- (get-node-size node)))
     finally (return new)))

(defun insert-helper (node key record)
  "Inserts a key/record into a tree, passing up new nodes if splits ocurred."
  (flet ((add-record (node key record)
           (let ((index (search-node-keys node key)))
             (move-records node (search-node-keys node key))
             (set-node-key-record node index key record)
             (set-node-size node (1+ (get-node-size node)))))
         (add-key (node new-node)
            (let* ((new-key (get-node-key new-node 0))
                   (index (search-node-keys node new-key)))
              (move-records node index)
              (set-node-key-record node index new-key (get-node-record node (1+ index)))
              (set-node-record node (1+ index) new-node)
              (set-node-size node (1+ (get-node-size node))))))
    (if (is-node-p node)
        (let ((new-node (insert-helper (find-node node key) key record))) ; Go down the tree.
          (when new-node                                                  ; Do we have a split?                                     ; Do we have a split?
            (add-key node new-node)))
        (let ((update (search-node-keys node key :record-search t)))      ; Is this an update?
          (cond (update (set-node-key-record node update key record) nil) ; Update and return nil.
                (t (add-record node key record)                           ; Insert, add record.
                   (when (is-node-illegal-p node)
                     (split-node node))))))))                             ; Split leaf? Return new node.

;;; Public interface

(defun search-tree (tree key)
  "Search for a record in the given tree using the given key."
  (if (is-node-p tree)
      (search-tree (find-node tree key) key)
      (find-record tree key)))

(defun insert-tree (tree key record)
  "Insert a record into the given tree using the given key."
  (insert-helper tree key record))

;;; Testing code

(defun fake-tree ()
  "Create a b+ tree by manually for testing."
  (let ((tree (make-node 4 :node))
        (first-node (make-node 4 :leaf))
        (second-node (make-node 4 :leaf))
        (third-node (make-node 4 :leaf)))
    (set-node-key-record first-node 0 1 "1")
    (set-node-key-record second-node 0 3 "3")
    (set-node-key-record second-node 1 4 "4")
    (set-node-key-record second-node 2 5 "5")
    (set-node-key-record third-node 0 17 "17")
    (set-node-key-record third-node 1 30 "30")
    (set-node-key-record tree 0 3 first-node)
    (set-node-key-record tree 1 17 second-node)
    (set-node-record tree 2 third-node)
    (set-node-size first-node 1)
    (set-node-size second-node 3)
    (set-node-size third-node 2)
    (set-node-size tree 3)
    tree))

(defparameter *a* nil)

(defun runtest ()
  (defparameter *a* (fake-tree))
  (insert-tree *a*   6 "6")
  (insert-tree *a*  -1 "-1")
  (insert-tree *a*   2 "2")
  (insert-tree *a*  50 "50")
  (insert-tree *a* -20 "-20")
  (insert-tree *a* -50 "-50")
  (insert-tree *a* -10 "-10")
  (insert-tree *a* -60 "-60")
  (insert-tree *a*  60 "60"))
