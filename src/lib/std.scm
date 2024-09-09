(defunc map [func lst] (if (== '() lst) '() (cons (func (car lst)) (map func (cdr lst)))))

(defunc filter [predicate lst]
  (if (== lst '()) 
    '()
    (if (predicate (car lst))
      (cons (car lst) (filter predicate (cdr lst)))
      (filter predicate (cdr lst)))))



let filter = (=> : List<U> | func : Fn<T:U>, lst : List<T> | {
               ((== lst '())
                 -> (lst)
                 : (cons (func(car lst)) (filter func (cdr lst))))
               
               })


func map[func: fn<T:U> lst: List<T>] -> List<U> {
  ((== lst '())
    -> (lst)
    : (cons func[(car lst)] (filter[ func (cdr lst)]))
    : cons[func[car[lst]], filter[func[cdr[lst]]]]
}
    
    let x = (element |e| -> ns->OtherObj[e] )
    (define x (init OtherObject element))
    
    let x = (SumFunc [10 20] -> |(x1 . x2) | {
                  let (x . y) = ((^ x1 2) . (^ x2 2))
                  (/ y x)
              } 
              : {
                  let (x y) = ((^ 10 2) . (^ 20 2))
                  (/ y x)
              })
    
    
    
    (define x (OtherFunc (SumFunc 10 20)))