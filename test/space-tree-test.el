;;; space-tree-test.el --- Tests for space-tree -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;;; Commentary:

;; ERT tests for space-tree.

;;; Code:

(require 'ert)
(require 'ht)
(require 'dash)
(require 'space-tree)


;;; Test Infrastructure

(defmacro space-tree-test-with-clean-state (&rest body)
  "Execute BODY with all space-tree global state saved and restored."
  (declare (indent 0) (debug t))
  `(let ((space-tree-tree (ht-create))
         (space-tree-current-address '())
         (space-tree-address-wconf-tbl (ht-create))
         (space-tree-recent-space-list '())
         (space-tree-space-name-tbl (ht-create))
         (space-tree-copied-space nil))
     ,@body))

(defmacro space-tree-test-with-mock-windows (&rest body)
  "Execute BODY with window-related functions stubbed out."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'space-tree--select-non-side-window) #'ignore)
             ((symbol-function 'window-state-get) (lambda (&rest _) '(mock-wconf)))
             ((symbol-function 'space-tree--window-state-put-safely) #'ignore)
             ((symbol-function 'delete-other-windows) #'ignore)
             ((symbol-function 'switch-to-buffer) #'ignore)
             ((symbol-function 'force-mode-line-update) #'ignore))
     ,@body))

(defun space-tree-test-build-tree (addresses)
  "Populate space-tree state from a list of ADDRESSES.
Each address is a list of integers.  Sets up tree structure, wconf
table entries, current-address (last in list), and recent-space-list."
  (ht-set space-tree-tree "space-tree" (ht-create))
  (dolist (addr addresses)
    ;; Ensure all intermediate nodes exist
    (let ((path '()))
      (dolist (key addr)
        (setq path (append path (list key)))
        (unless (space-tree--get path)
          (space-tree--set path))))
    ;; Store a placeholder wconf
    (ht-set space-tree-address-wconf-tbl addr `(mock-wconf ,addr)))
  ;; Current address is the last one given
  (setq space-tree-current-address (car (last addresses)))
  ;; Recent list is addresses in reverse order
  (setq space-tree-recent-space-list (reverse addresses)))


;;; A. Pure Functions

;; space-tree--list-starts-with

(ert-deftest space-tree-test-list-starts-with/match ()
  (should (space-tree--list-starts-with '(1 2 3) '(1 2))))

(ert-deftest space-tree-test-list-starts-with/exact ()
  (should (space-tree--list-starts-with '(1 2) '(1 2))))

(ert-deftest space-tree-test-list-starts-with/no-match ()
  (should-not (space-tree--list-starts-with '(1 2 3) '(1 3))))

(ert-deftest space-tree-test-list-starts-with/empty-sub ()
  (should (space-tree--list-starts-with '(1 2 3) '())))

(ert-deftest space-tree-test-list-starts-with/sub-longer ()
  (should-not (space-tree--list-starts-with '(1) '(1 2))))

(ert-deftest space-tree-test-list-starts-with/both-empty ()
  (should (space-tree--list-starts-with '() '())))

;; space-tree--remove-adjacent-duplicates

(ert-deftest space-tree-test-remove-adj-dups/basic ()
  (should (equal (space-tree--remove-adjacent-duplicates '(1 1 2 3 3))
                 '(1 2 3))))

(ert-deftest space-tree-test-remove-adj-dups/no-dups ()
  (should (equal (space-tree--remove-adjacent-duplicates '(1 2 3))
                 '(1 2 3))))

(ert-deftest space-tree-test-remove-adj-dups/all-same ()
  (should (equal (space-tree--remove-adjacent-duplicates '(1 1 1))
                 '(1))))

(ert-deftest space-tree-test-remove-adj-dups/empty ()
  (should (equal (space-tree--remove-adjacent-duplicates '())
                 '())))

(ert-deftest space-tree-test-remove-adj-dups/non-adjacent ()
  "Non-adjacent duplicates are preserved."
  (should (equal (space-tree--remove-adjacent-duplicates '(1 2 1 2))
                 '(1 2 1 2))))

(ert-deftest space-tree-test-remove-adj-dups/list-elements ()
  "Works with list elements (addresses)."
  (should (equal (space-tree--remove-adjacent-duplicates
                  '((1 2) (1 2) (3 4)))
                 '((1 2) (3 4)))))


;;; B. State-Reading Functions

;; space-tree--current-depth

(ert-deftest space-tree-test-current-depth/level-1 ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1)))
      (should (= (space-tree--current-depth) 1)))))

(ert-deftest space-tree-test-current-depth/level-3 ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1 2 3)))
      (should (= (space-tree--current-depth) 3)))))

;; space-tree--current-parent

(ert-deftest space-tree-test-current-parent/has-parent ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1 2)))
      (should (equal (space-tree--current-parent) '(1))))))

(ert-deftest space-tree-test-current-parent/top-level ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1)))
      (should (equal (space-tree--current-parent) '())))))

(ert-deftest space-tree-test-current-parent/deep ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1 2 3 4)))
      (should (equal (space-tree--current-parent) '(1 2 3))))))

;; space-tree--space-exists-p

(ert-deftest space-tree-test-space-exists-p/exists ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2)))
      (should (space-tree--space-exists-p '(1))))))

(ert-deftest space-tree-test-space-exists-p/not-exists ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1)))
      (should-not (space-tree--space-exists-p '(5))))))

;; space-tree--number-of-spaces-current-level

(ert-deftest space-tree-test-number-of-spaces/single ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1)))
      (should (= (space-tree--number-of-spaces-current-level) 1)))))

(ert-deftest space-tree-test-number-of-spaces/multiple ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2) (3)))
      (should (= (space-tree--number-of-spaces-current-level) 3)))))

(ert-deftest space-tree-test-number-of-spaces/nested ()
  "Count only spaces at the current level, not siblings at parent level."
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (1 1) (1 2) (1 3)))
      ;; current-address is (1 3), parent ht at (1) has keys 1,2,3
      (should (= (space-tree--number-of-spaces-current-level) 3)))))

;; space-tree--recent-space-on-path

(ert-deftest space-tree-test-recent-space-on-path/found ()
  (space-tree-test-with-clean-state
    (setq space-tree-recent-space-list '((1 2 3) (1 2) (2 1)))
    (should (equal (space-tree--recent-space-on-path '(1 2)) '(1 2 3)))))

(ert-deftest space-tree-test-recent-space-on-path/not-found ()
  (space-tree-test-with-clean-state
    (setq space-tree-recent-space-list '((1 2 3) (2 1)))
    (should-not (space-tree--recent-space-on-path '(3)))))

(ert-deftest space-tree-test-recent-space-on-path/exact-match ()
  (space-tree-test-with-clean-state
    (setq space-tree-recent-space-list '((1 2) (3 4)))
    (should (equal (space-tree--recent-space-on-path '(1 2)) '(1 2)))))

;; space-tree--get

(ert-deftest space-tree-test-get/top-level ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1)))
      (should (ht-p (space-tree--get '(1)))))))

(ert-deftest space-tree-test-get/nested ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1 2)))
      (should (ht-p (space-tree--get '(1 2)))))))

(ert-deftest space-tree-test-get/nonexistent ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1)))
      (should-not (space-tree--get '(9))))))

;; space-tree--get-parent-ht-at

(ert-deftest space-tree-test-get-parent-ht-at/nested ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (1 2)))
      (let ((parent (space-tree--get-parent-ht-at '(1 2))))
        (should (ht-p parent))
        (should (ht-contains-p parent 2))))))

(ert-deftest space-tree-test-get-parent-ht-at/top-level ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2)))
      (let ((parent (space-tree--get-parent-ht-at '(1))))
        (should (ht-p parent))
        (should (ht-contains-p parent 1))
        (should (ht-contains-p parent 2))))))


;;; C. State-Mutation Functions

;; space-tree--set

(ert-deftest space-tree-test-set/new-top-level ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (ht-set space-tree-tree "space-tree" (ht-create))
      (space-tree--set '(1))
      (should (ht-p (space-tree--get '(1)))))))

(ert-deftest space-tree-test-set/nested ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (ht-set space-tree-tree "space-tree" (ht-create))
      (space-tree--set '(1))
      (space-tree--set '(1 2))
      (should (ht-p (space-tree--get '(1 2)))))))

(ert-deftest space-tree-test-set/deep ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (ht-set space-tree-tree "space-tree" (ht-create))
      (space-tree--set '(1))
      (space-tree--set '(1 2))
      (space-tree--set '(1 2 3))
      (should (ht-p (space-tree--get '(1 2 3)))))))

;; space-tree--remove

(ert-deftest space-tree-test-remove/basic ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2)))
      (space-tree--remove '(2))
      (should-not (space-tree--get '(2)))
      (should-not (ht-get space-tree-address-wconf-tbl '(2))))))

(ert-deftest space-tree-test-remove/nested ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (1 1) (1 2)))
      (setq space-tree-current-address '(1 2))
      (space-tree--remove '(1 1))
      (should-not (space-tree--get '(1 1)))
      ;; Parent still has remaining child
      (should (ht-p (space-tree--get '(1 2)))))))

(ert-deftest space-tree-test-remove/cleans-name ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2)))
      (ht-set space-tree-space-name-tbl '(2) "work")
      (space-tree--remove '(2))
      (should-not (ht-get space-tree-space-name-tbl '(2))))))

(ert-deftest space-tree-test-remove/cleans-history ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2)))
      (setq space-tree-recent-space-list '((2) (1)))
      (space-tree--remove '(2))
      (should-not (member '(2) space-tree-recent-space-list)))))

;; space-tree--process-history

(ert-deftest space-tree-test-process-history/removes-prefix ()
  "Parent addresses are removed when a child is in the list."
  (space-tree-test-with-clean-state
    (setq space-tree-recent-space-list '((1 2 3) (1 2) (1)))
    (space-tree--process-history)
    (should (equal space-tree-recent-space-list '((1 2 3))))))

(ert-deftest space-tree-test-process-history/keeps-non-prefix ()
  (space-tree-test-with-clean-state
    (setq space-tree-recent-space-list '((1 2) (3 4)))
    (space-tree--process-history)
    (should (equal space-tree-recent-space-list '((1 2) (3 4))))))

(ert-deftest space-tree-test-process-history/removes-adj-dups ()
  (space-tree-test-with-clean-state
    (setq space-tree-recent-space-list '((1 2) (1 2) (3 4)))
    (space-tree--process-history)
    (should (equal space-tree-recent-space-list '((1 2) (3 4))))))

;; space-tree-init

(ert-deftest space-tree-test-init/default ()
  "Default init starts at space 1."
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (let ((space-tree-start-at-0 nil))
        (space-tree-init)
        (should (equal space-tree-current-address '(1)))
        (should (ht-get space-tree-address-wconf-tbl '(1)))
        (should (ht-p (space-tree--get '(1))))))))

(ert-deftest space-tree-test-init/start-at-0 ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (let ((space-tree-start-at-0 t))
        (space-tree-init)
        (should (equal space-tree-current-address '(0)))
        (should (ht-get space-tree-address-wconf-tbl '(0)))))))

(ert-deftest space-tree-test-init/resets-state ()
  "Init clears any pre-existing state."
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2) (3)))
      (ht-set space-tree-space-name-tbl '(1) "old")
      (let ((space-tree-start-at-0 nil))
        (space-tree-init)
        ;; Old spaces gone
        (should-not (ht-get space-tree-address-wconf-tbl '(2)))
        (should-not (ht-get space-tree-address-wconf-tbl '(3)))
        ;; Name table cleared
        (should (= (ht-size space-tree-space-name-tbl) 0))))))

;; space-tree-switch

(ert-deftest space-tree-test-switch/basic ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2)))
      (setq space-tree-current-address '(1))
      (space-tree-switch '(2))
      (should (equal space-tree-current-address '(2))))))

(ert-deftest space-tree-test-switch/history-updated ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2)))
      (setq space-tree-current-address '(1))
      (setq space-tree-recent-space-list '((1)))
      (space-tree-switch '(2))
      (should (equal (car space-tree-recent-space-list) '(2))))))

;; space-tree-switch-or-create

(ert-deftest space-tree-test-switch-or-create/existing ()
  "Switching to existing space via recent path."
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2)))
      (setq space-tree-current-address '(1))
      (setq space-tree-recent-space-list '((1) (2)))
      (space-tree-switch-or-create '(2))
      (should (equal space-tree-current-address '(2))))))

(ert-deftest space-tree-test-switch-or-create/new-space ()
  "Switching to non-existent address creates it."
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1)))
      (setq space-tree-current-address '(1))
      (space-tree-switch-or-create '(2))
      (should (equal space-tree-current-address '(2)))
      (should (ht-get space-tree-address-wconf-tbl '(2))))))

(ert-deftest space-tree-test-switch-or-create/via-recent ()
  "Navigating to a parent address resolves via recent list."
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (1 1) (1 2)))
      (setq space-tree-current-address '(1))
      (setq space-tree-recent-space-list '((1 2) (1 1) (1)))
      (space-tree-switch-or-create '(1))
      ;; Should resolve to (1 2) — first recent space on path (1)
      (should (equal space-tree-current-address '(1 2))))))


;;; D. Modeline Rendering

(ert-deftest space-tree-test-modeline/single-space ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1)))
      (let ((result (space-tree-modeline-lighter)))
        (should (stringp result))
        (should (string-match-p "1'" result))
        (should (string-match-p "^{" result))
        (should (string-match-p "}$" result))))))

(ert-deftest space-tree-test-modeline/two-spaces ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2)))
      ;; current address is (2)
      (let ((result (space-tree-modeline-lighter)))
        (should (stringp result))
        ;; active space 2 should have bold marker
        (should (string-match-p "2'" result))
        ;; space 1 should be present without bold
        (should (string-match-p "1 " result))))))

(ert-deftest space-tree-test-modeline/nested ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (1 1) (1 2)))
      ;; current address is (1 2)
      (let ((result (space-tree-modeline-lighter)))
        (should (stringp result))
        ;; should have pipe separator for nesting
        (should (string-match-p "|" result))))))

(ert-deftest space-tree-test-modeline/with-name ()
  (space-tree-test-with-clean-state
    (space-tree-test-with-mock-windows
      (space-tree-test-build-tree '((1) (2)))
      (ht-set space-tree-space-name-tbl '(2) "work")
      (let ((result (space-tree-modeline-lighter)))
        (should (string-match-p "work" result))))))

(provide 'space-tree-test)

;;; space-tree-test.el ends here
