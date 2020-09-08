(setq suites nil)

(cl-defstruct suite
  (id nil)
  (groups nil))

(cl-defstruct group
  (id nil)
  (groups nil)
  (tests nil))

(cl-defstruct test
  (id nil)
  (name 'non-name))

(add-to-list 'suites (cons 1 (make-suite :id 1)))

(defun set-child-group (parent-group group-id parent-id)
  "."
  (let ((groups (group-groups parent-group)))
    (if (seq-empty-p groups)
        (setf (alist-get group-id (group-groups parent-group))
              (make-group :id group-id))
      (setf (group-groups parent-group)
            (seq-map (lambda (g)
                       (if (= (car g) group-id)
                           (cons (car g)
                                 (setf (alist-get group-id (group-groups (cdr g)))
                                       (make-group :id group-id)))
                         (if (= (car g) parent-id)
                             (cons (car g) (make-group :id group-id))
                           (set-child-group (cdr g)
                                            group-id
                                            parent-id))))
                     groups)))
    parent-group))

(defun set-group (suite-id group-id &optional parent-id)
  (let* ((suite (alist-get suite-id suites))
         (parent-group (alist-get parent-id (suite-groups suite))))
    (if parent-id
        (setf (alist-get parent-id (suite-groups suite))
              (set-child-group parent-group
                               group-id
                               parent-id))
      (setf (alist-get group-id (suite-groups suite))
            (make-group :id group-id)))))

(defun get-group (suite-id group-id)
  (when-let (suite (alist-get suite-id suites))
    (alist-get group-id (suite-groups suite))))


(setq suites nil)
(add-to-list 'suites (cons 1 (make-suite :id 1)))
(set-group 1 2)
(set-group 1 4 2)

suites

(get-group 1 2)

(defun set-test (suite-id group-id test-id)
  (let ((suite (alist-get suite-id suites)))
    (let ((group (alist-get group-id (suite-groups suite))))
      (setf (alist-get test-id (group-tests group))
            (make-test :id test-id)))))

(set-test 1 2 4)
