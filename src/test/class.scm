(def-class MyClass
  (:param &mut)
  (:var
    (time &mut ::LocalDateTime)
    (uuid ::UUID "234234234sdf")
    (name ::String)
    (address ::String))

  (:pre
    (:= name 10)
    (:= address "String"))

  (:init
    ([name ::string address ::string]
      (:= name name)
      (:= address address)
      (:= time 0))

    ([] ()))

  (:validate
    (!= uuid #null)
    (== address "address")
    (> 10 time))

  (:func
    (get-time [] time)

    (get-time2 [ignore] time)

    (set-time [arg] (:= time arg))

    (times-2 [x]
      (* x 2))))


