(defsystem "afp-forth-demo"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("alexandria" "serapeum")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "small forth compiler"
  :in-order-to ((test-op (test-op "afp-forth-demo/tests"))))

(defsystem "afp-forth-demo/tests"
  :author ""
  :license ""
  :depends-on ("afp-forth-demo"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for afp-forth-demo"
  :perform (test-op (op c) (symbol-call :rove :run c)))
