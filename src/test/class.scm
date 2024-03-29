(def-class MyClass
  (:param &mut)
  (:var
    (time &mut ::LocalDateTime)
    (uuid ::UUID)
    (name ::String)
    (address ::String))

  (:pre
    (:= uuid 10)
    (:= name 10)
    (:= address "String"))

  (:init
    ([name address]
      (:= name name)
      (:= address address))

    ([name address]
      (:= name name)
      (:= address address))

    ([name address]
      (:= name name)
      (:= address address))

    ([] ()))

  (:validate
    (!= uuid #nil)
    (> (address:.len) 20))

  (:func
    (get-address-formated [formatter ::formatter]
      (try ((formatter address)) (catch IOException (e))))))



