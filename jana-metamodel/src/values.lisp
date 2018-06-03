(in-package :JANA.METAMODEL)

(defclass jana-value ()
  ((jana-type
    :TYPE jana-type
    :DOCUMENTATION "The type of the value."))
  (:DOCUMENTATION "A concrete value."))

(defclass jana-constant-value (jana-value)
  ()
  (:DOCUMENTATION "A constant value."))


(defclass jana-null-value (jana-constant-value)
  ()
  (:DOCUMENTATION "The value representing no value -- NULL."))

