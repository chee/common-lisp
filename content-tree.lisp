(defpackage :content-tree (:use :cl))
(in-package :content-tree)

(defun node (type keywords)
  "Create an ordinary node."
  (setf (getf keywords :type) type)
  keywords)

(defun parent (type keywords &rest children)
  "Create a node with children."
  (node type (append keywords `(:children ,@children))))

(defun text (value)
  "Create a text node."
  (list :type "text" :value value))

(defun split-keywords-and-children (args)
  "Given a list (ARGS) containing key-val pairs followed by an arbitrary number of
other values, return a pair of lists. Car is a property-list and cadr is
everything else."
  (let (plist children prev-key (i 0))
    (dolist (arg args)
      (cond
        (prev-key
          (setq plist (append plist (list prev-key arg)))
          (setf prev-key nil))
        ((keywordp arg)
          (setq prev-key arg))
        (t (setq children (append children (list arg))))))
    (list plist children)))

(defun string-type (type)
  "Convert a type symbol to its content-tree name."
  (string-downcase (string type)))

(defun create (type &rest keywords-and-children)
  "Create a content-tree structure from a nice sexp format."
  (if (eql type 'text)
      (text (first keywords-and-children))
      (DESTRUCTURING-BIND (keywords children) (split-keywords-and-children keywords-and-children)
        (print keywords)
        (if children
          (parent (string-type type) keywords
            (mapcar #'(lambda (child) (apply #'create child)) children))
          (node (string-type type) keywords)))))

(defun content-tree (&rest nodes)
  "Create a content-tree structure by typing slightly fewer ticks and quotes."
  (apply #'create `(root ,@nodes)))

(defun node-to-table (node)
  "Recursively a content tree node to a table suitable for `json-lib'."
  (let ((table (make-hash-table)))
    (loop for (key value) on node by #'cddr
      do (let ((prop (kebab:to-camel-case (string key))))
           (if (eql key :children)
             (setf (gethash prop table) (mapcar #'node-to-table value))
             (setf (gethash prop table) value))))
    table))

(defun to-json (node)
  "Convert a content-tree structure to content-tree json."
  (json-lib:stringify (node-to-table node)))

(defun save-to-json (filename node)
  "Save a content-tree node as content-tree json."
   (with-open-file
      (out filename :direction :output :if-exists :supersede)
      (with-standard-io-syntax (princ (to-json node) out))))

(defun test (save-to-json "article.json"
 (content-tree
   '(topper
      (headline (text "This is the news"))
      (summary (text "Pretty cool news")))
   '(body
      (paragraph (text "look"))
      (scrolly-block
        :theme "sans"
        :layout-width "full-grid"
        (scrolly-section
          :display "no-box"
          (scrolly-image :url "https://whatever")
          (scrolly-copy
            (scrolly-text
              :level "chapter"
              (text "hey")))))))))
