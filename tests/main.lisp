(defpackage afp-forth-demo/tests/main
  (:use :cl
        :afp-forth-demo
        :rove))
(in-package :afp-forth-demo/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :afp-forth-demo)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
