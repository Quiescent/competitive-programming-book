(defsystem "competitive-programming-v2"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:cl-ppcre :iterate :printv)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "competitive-programming-v2/tests"))))

(defsystem "competitive-programming-v2/tests"
  :author ""
  :license ""
  :depends-on ("competitive-programming-v2"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for competitive-programming-v2"
  :perform (test-op (op c) (symbol-call :rove :run c)))
