;;; General purpose in-memory B+ tree.
;;;

;;; General access functions

(defun get-node-type (node)
  "Get the B+ tree node type."
  (nth 0 node))

(defun set-node-type (node type)
  "Set the B+ tree node type."
  (setf (nth 0 node) type))

(defun get-node-order (node)
  "Get the B+ tree node order."
  (nth 1 node))

(defun get-node-size (node)
  "Get the B+ tree node order."
  (nth 2 node))

(defun set-node-size (node size)
  "Set the B+ tree node order."
  (setf (nth 2 node) size))

(defun get-node-keys (node)
  "Get the node keys vector."
  (nth 3 node))

(defun get-node-records (node)
  "Get the node records vector."
  (nth 4 node))

(defun get-node-next-node (node)
  "Get the next node following the linked list."
  (nth 5 node))

(defun set-node-next-node (node next-node)
  "Set the next node in the linked list."
  (setf (nth 5 node) next-node))

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

(defun set-node-key-record (node n key record)
  "Sets both the key and record at the given index  to the given B+ node."
  (set-node-key node n key)
  (set-node-record node n record))

(defun is-node-p (node)
  "Is the node an internal node?"
  (eq :node (get-node-type node)))

(defun is-leaf-p (node)
  "Is the node a leaf?"
  (eq :leaf (get-node-type node)))

(defun is-node-full-p (node)
  "Is the node full? (Leaf nodes are full at order - 1)."
  (>= (get-node-size node)
      (- (get-node-order node) (if (is-leaf-p node) 1 0 ))))

;;; Internal tree operations

(defun search-node-keys (node key &key record-search)
  "Search the given node keys vector using binary search. Keys assumed to be sorted. Optional mix and max define the search space."
  (labels ((binary-search (min max)
             (if (< max min)
                 (unless record-search (1+ max))
                 (let* ((mid (+ min (ash (- max min) -1)))
                        (k (get-node-key node mid)))
                   (cond ((< key k) (binary-search min (1- mid)))
                         ((> key k) (binary-search (1+ mid) max))
                         (t (+ mid (if record-search 0 1))))))))
    (binary-search 0 (1- (get-node-size node)))))

(defun find-record (node key)
  "Get the record with the given key in the given node, nil if none."
  (let ((index (search-node-keys node key :record-search t)))
    (unless (null index)
      (get-node-record node index))))

(defun find-node (node key)
  "Get the next node using the given key in the given node."
  (get-node-record node (search-node-keys node key)))

(defun find-leaf-node (tree key)
  "Return the leaf node down the B+ tree for the given key."
  (if (is-node-p tree)
      (find-leaf-node (find-node tree key) key)
      tree))

(defun move-records (node index)
  "Move the keys and records from the given starting point to the right."
  (let ((max (get-node-size node)))
    (loop for i from max downto index with j = (1- i) do
         (set-node-key-record node i (get-node-key node j) (get-node-record node j)))
    (set-node-key-record node index nil nil)))

(defun make-node (order &optional (type :node))
  "Makes an empty B+ tree node with the given order and the optional type (:leaf or :node)."
  (list type
        order
        0
        (make-array (1- order) :initial-element nil)
        (make-array order :initial-element nil)
        nil))

;;; Public interface

(defun insert-to-tree (tree key record)
  "Add a record with the given key to the given tree."
  (labels ((add-record (node)
             (let ((index (search-node-keys node key)))
               (move-records node (search-node-keys node key))
               (set-node-key-record node index key record))
             (set-node-size node (1+ (get-node-size node)))
             record))
    (let ((node (find-leaf-node tree key)))
      (if (is-node-full-p node)
          nil
          (add-record node)))))

(defun search-tree (tree key)
  "Search for a record in the given tree using the given key."
  (find-record (find-leaf-node tree key) key))

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
    (set-node-key-record third-node 0 5 "5")
    (set-node-key-record third-node 1 6 "6")
    (set-node-key-record tree 0 3 first-node)
    (set-node-key-record tree 1 5 second-node)
    (set-node-record tree 2 third-node)
    (set-node-size first-node 1)
    (set-node-size second-node 2)
    (set-node-size third-node 2)
    (set-node-size tree 2)
    tree))
