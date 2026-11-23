(defpackage #:midi-protocol/tests
  (:use #:cl #:rove))

(in-package #:midi-protocol/tests)

(deftest skeleton-loads
  (ok (find-package '#:midi-protocol)))
