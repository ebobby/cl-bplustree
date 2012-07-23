;;; General purpose in-memory B+ tree.
;;;

;;; General access functions

(defun get-node-type (node)
  "Get the node type."
  (nth 0 node))

(defun get-node-order (node)
  "Get the node order."
  (nth 1 node))

(defun get-node-size (node)
  "Get the node order."
  (nth 2 node))

(defun get-node-keys (node)
  "Get the node keys vector."
  (nth 3 node))

(defun get-node-records (node)
  "Get the node records vector."
  (nth 4 node))

(defun get-node-next-node (node)
  "Get the next node."
  (nth 5 node))

(defun set-node-type (node type)
  (setf (nth 0 node) type))

(defun set-node-size (node size)
  (setf (nth 2 node) size))

(defun is-node-p (node)
  "Is the node an internal node?"
  (eq :node (get-node-type node)))

(defun is-leaf-p (node)
  "Is the node a leaf?"
  (eq :leaf (get-node-type node)))

(defun is-node-full-p (node)
  (>= (length (get-node-keys node)) (get-node-order node)))

;;; Internal tree operations

(defun search-node-keys (node key &key record-search)
  "Search the given node keys vector using binary search. Keys assumed to be sorted. Optional mix and max define the search space."
  (let ((keys (get-node-keys node)))
    (labels ((binary-search (min max)
               (if (< max min)
                   (unless record-search (1+ max))
                   (let* ((mid (+ min (ash (- max min) -1)))
                          (k (aref keys mid)))
                     (cond ((< key k) (binary-search min (1- mid)))
                           ((> key k) (binary-search (1+ mid) max))
                           (t (+ mid (if record-search 0 1))))))))
      (binary-search 0 (1- (get-node-size node))))))

(defun find-record (node key)
  "Get the record with the given key in the given node, nil if none."
  (let ((index (search-node-keys node key :record-search t)))
    (unless (null index)
      (aref (get-node-records node) index))))

(defun find-node (node key)
  "Get the next node using the given key in the given node."
  (aref (get-node-records node)
        (search-node-keys node key)))

(defun find-leaf-node (tree key)
  (if (is-node-p tree)
      (find-leaf-node (find-node tree key) key)
      tree))

;;; Public interface

 ;; (defun insert-to-tree (tree key record)
 ;;   "Add a record with the given key to the given tree."
 ;;   (labels ((add-record (node)
 ;;              (setf (aref (get-node-records node)
 ;;                          (vector-push key (get-node-keys node)))
 ;;                    record)))
 ;;     (let ((node (find-leaf-node tree key)))
 ;;       (if (is-node-full-p node)
 ;;           nil
 ;;           (add-record node)))))

(defun search-tree (tree key)
  "Search for a record in the given tree using the given key."
  (find-record (find-leaf-node tree key) key))

(defun make-node (order &optional (type :node))
  "Makes an empty B+ tree node with the given order and the optional type (:leaf or :node)."
  (list type
        order
        0
        (make-array (1- order) :initial-element nil)
        (make-array order :initial-element nil)
        nil))

;;; Testing code

(defun fake-tree ()
  "Create a b+ tree by hand."
  (let ((tree (make-node 4 :node))
        (first-node (make-node 4 :leaf))
        (second-node (make-node 4 :leaf))
        (third-node (make-node 4 :leaf)))
    (setf (aref (get-node-keys first-node) 0) 1)
    (setf (aref (get-node-records first-node) 0) "1")
    (setf (aref (get-node-keys second-node) 0) 3)
    (setf (aref (get-node-keys second-node) 1) 4)
    (setf (aref (get-node-records second-node) 0) "3")
    (setf (aref (get-node-records second-node) 1) "4")
    (setf (aref (get-node-keys third-node) 0) 5)
    (setf (aref (get-node-keys third-node) 1) 6)
    (setf (aref (get-node-keys third-node) 2) 7)
    (setf (aref (get-node-records third-node) 0) "5")
    (setf (aref (get-node-records third-node) 1) "6")
    (setf (aref (get-node-records third-node) 2) "7")
    (setf (aref (get-node-keys tree) 0) 3)
    (setf (aref (get-node-keys tree) 1) 5)
    (setf (aref (get-node-records tree) 0) first-node)
    (setf (aref (get-node-records tree) 1) second-node)
    (setf (aref (get-node-records tree) 2) third-node)
    (set-node-size first-node 1)
    (set-node-size second-node 2)
    (set-node-size third-node 3)
    (set-node-size tree 2)
    tree))
