(defunc map [func lst] (if (== '() lst) '() (cons (func (car lst)) (map func (cdr lst)))))

(defunc filter [predicate lst]
  (if (== lst '()) 
    '()
    (if (predicate (car lst))
      (cons (car lst) (filter predicate (cdr lst)))
      (filter predicate (cdr lst)))))


