(defsystem "midi-protocol"
  :version "0.0.1"
  :description "MIDI 1.0 event model and Standard MIDI File reader and writer for cl-stack"
  :author "egao1980"
  :license "MIT"
  :pathname "src"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "midi-protocol/tests"))))

(defsystem "midi-protocol/tests"
  :depends-on ("midi-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "midi-protocol-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
