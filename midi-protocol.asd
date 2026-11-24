(defsystem "midi-protocol"
  :version "0.1.0"
  :description "MIDI 1.0 event model and Standard MIDI File reader and writer for cl-stack"
  :author "egao1980"
  :license "MIT"
  :depends-on ("binary-protocol")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "model")
               (:file "time")
               (:file "read")
               (:file "write"))
  :in-order-to ((test-op (test-op "midi-protocol/tests"))))

(defsystem "midi-protocol/tests"
  :depends-on ("midi-protocol" "rove" "cl-stack-pathlib" "process-backend-uiop")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "model-test")
               (:file "codec-test")
               (:file "malformed-test")
               (:file "time-test")
               (:file "mido-test")
               (:file "property-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
