#lang pl

#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}
        |  { union <SOL> <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
       
<NumList> :: =  λ | <num> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------


;;///////--------------------------------------part A-----------------------------------------///////////
;; The abstract syntax tree SOL
#|

In this part we have implemented the SOL language,
The language we implemented is a language with three actions:

Union - between two lists and without repetitions and sorted.
Intersection- Union of lists with common accessories and sorting.
Scalar Multiplication - Each limb in the list is multiplied by the same scalar.
|#
(define-type SET = (Listof Number))

(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
    [Set  SET]
    [Smult Number SOL]
    [Inter SOL SOL]
    [Union SOL SOL]
    [IdS    Symbol]
    [WithS  Symbol SOL SOL])

;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable. 
(: parse-sexprS : Sexpr -> SOL)
;; to convert s-expressions into SOLs


(define (parse-sexprS sexpr);; In this function we implement the SOL language with the help of the language constructors.
  (match sexpr
    [(list (number: ns) ...) (Set ns)] ;;<-- fill in --> : (Set ns) set of numbers constructor Set
    [(symbol: name) (IdS name)] ;;<-- fill in --> : (IdS name) a symbol 
    [(cons 'with more)
     (match sexpr ;;<-- fill in -->  match for with and a list with symbol and body(SOL..)
          [(list 'with (list (symbol: name) named) body) (WithS name (parse-sexprS named) (parse-sexprS body))]
          [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexprS rhs))] ;;<-- fill in --> constructor (Smult number and SOL) 
    [(list 'intersect lhs rhs) (Inter (parse-sexprS lhs) (parse-sexprS rhs))];;<-- fill in --> constructor (Inter SOL and SOL) 
    [(list 'union lhs rhs) (Union (parse-sexprS lhs) (parse-sexprS rhs))];;<-- fill in --> constructor (Union SOL and SOL) 
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parseS : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parseS str)
  (parse-sexprS (string->sexpr str)))




(test (parseS "{1 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{with S {intersect {1 2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexpr: bad `with' syntax in")

(test (parseS "{intersect {union {1 2 3} {7 7 7}} {4 2 3}}")  => (Inter (Union (Set '(1 2 3)) (Set '(7 7 7))) (Set '(4 2 3))))
(test (parseS "{with {S {intersect {1 2 1 3 7 3} {union {1 2 3} {4 2 3}}}} {union S S}}") => (WithS 'S (Inter (Set '(1 2 1 3 7 3)) (Union (Set '(1 2 3)) (Set '(4 2 3)))) (Union (IdS 'S) (IdS 'S))))
(test (parseS "{with {S {intersect {1 2 3} {4 2 3}}} {with {x {4 5 7 6 9 8 8 8}} {union x S}}}") => (WithS 'S (Inter (Set '(1 2 3)) (Set '(4 2 3))) (WithS 'x (Set '(4 5 7 6 9 8 8 8)) (Union (IdS 'x) (IdS 'S)))))


;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

#|
The function ismember? : receives a number and a list,
and checks if the number belongs to the list.
|#
(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond
    [(null? l) #f]
    [(equal? n (first l)) #t]
    [else (ismember? n (rest l))]))


(test (ismember? 1 '(3 4 5)) => #f)
(test (ismember? 1 '()) => #f)
(test (ismember? 1 '(1)) => #t)

#|
The function remove-duplicates : receives a list and returns a list,
The function checks if there are duplicates with the help of a function ismember?,
and returns a list without duplicates.
|#
(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
 (cond
   [(null? l) null]
   [(ismember? (first l) (rest l)) (remove-duplicates (rest l))]
   [else (append (list (first l)) (remove-duplicates (rest l)))]))



(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())
(test (remove-duplicates '(3 4 5 2 1 2 3 4)) => '(5 1 2 3 4))
(test (remove-duplicates '(2 2 2 2 2)) => '(2))


#|
The function create-sorted-set : receives a list and returns a list,
The function sorts the list in ascending order,
and deletes duplicates of numbers in the list using a function remove-duplicates
|#
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (cond
    [(null? l) null]
    [else (remove-duplicates (sort l <))]))


(test (create-sorted-set '(3 3 4 7 2 1 6 5 5 2 11)) => '(1 2 3 4 5 6 7 11))
(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '(3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())

#|
The function set-union : receives two lists and returns a consolidated list to one list,
and deletes duplicates and sorts the list in ascending order.
|#
(: set-union : SET SET -> SET)
(define (set-union A B)
  (create-sorted-set (append A B)))



(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))
(test (set-union '(5 6 3 2 2 3 4 6) '(3 5 6 1 7 8)) => '(1 2 3 4 5 6 7 8))


#|
The function set-intersection : receives two lists and returns a unified list,
with common members between the two lists and sorted.
|#
(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (create-sorted-set(filter mem-filter B)))


(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 3) '(3 3 4 5)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())
(test (set-intersection '(3 5 6 1 7 8 12) '(5 6 3 12 2 2 3 4 6)) => '(3 5 6 12))

#|
The function set-smult : receives a number and a list and returns a list,
that each member of the list is multiplied by a number(scalar).
|#
(: set-smult : Number (Listof Number) -> SET)
(define (set-smult n l)
  (cond
    [(null? l) null]
    [else (append (list (* n (first l))) (set-smult n (rest l)))]))


(test (set-smult 3 '(3 4 5)) => '(9 12 15))
(test (set-smult 2 '()) => '())
(test (set-smult 0 '(3 4 5)) => '(0 0 0))
(test (set-smult 1 '(3 2 4 6)) => '(3 2 4 6))
(test (set-smult 0 '(0 0 0))  => '(0 0 0))


;;-----------------------------------------------------
;; Substation 
#|
------------------------------------------------------
 Formal specs for `subst':
   (`Set' is a <NumList>, E, E1, E2 are <SOL>s, `x' is some <id>,
   `y' is a *different* <id>)
      Set[v/x]              = Set
      {Smult n E}[v/x]      = {Smult n E[v/x]}
      {Inter E1 E2}[v/x]    = {Inter E1[v/x] E2[v/x]}
      {Union E1 E2}[v/x]    = {Union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

#|
In the substS function:
Receives SOL symbol SOL and returns SOL,
The function looks for the equals of the symbols,
in order to assign a list to each symbol and finally use one of the actions on the symbols.
|#

(: substS : SOL Symbol SOL -> SOL)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (substS expr from to)
  (cases expr
    [(Set n) expr]
    [(Smult n s) (Smult n (substS s from to))]
    [(Inter l r) (Inter (substS l from to) (substS r from to))]
    [(Union l r) (Union (substS l from to) (substS r from to))]
    [(IdS name) (if (eq? name from) to expr)]
    [(WithS bound-id named-expr bound-body)
     (WithS bound-id (substS named-expr from to)
           (if (eq? bound-id from) bound-body
               (substS bound-body from to)))]))

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    
    eval({ N1 N2 ... Nl })  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) =  (K*N1 K*N2 ... K*Nl) 
    eval({intersect E1 E2}) = (sort (create-set (set-intersection (eval(E1) , eval(E2))))
                
    eval({union E1 E2}) = (sort (create-set (eval(E1) , eval(E2))))
    eval({with {x E1} E2}) = eval(E2[eval(E1)/x]) 
|#




;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 

#|
In the eval function:
Gets the language(SOL) and returns a list of numbers corresponding to the created constructs.
|#
(: eval : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr )
  (cases expr
    [(Set S) (create-sorted-set S)]  ;; sort and remove-duplicates
    [(Smult n set) (set-smult n (eval set))]
    [(Inter l r) (set-intersection (eval l) (eval r))]
    [(Union l r) (set-union (eval l) (eval r))]
    [(WithS name named body) (eval (substS body name named))]
    [(IdS name) (error 'eval "free identifier: ~s" name)]))




#|
In the run function:
We use the parseS function-
In order to get the language we have implemented (SOL),
and then we use the actions of the constructor with the functions we have implemented.
and we return a list.
|#


(: run : String -> SET)
;; evaluate a SOL program contained in a string
(define (run str)
  (eval (parseS str)))


(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))

(test (run "{with {S {intersect {1 2 3} {4 2 3}}} {with {x {4 5 7 6 9 8 8 8}} {union x S}}}") => '(2 3 4 5 6 7 8 9))


(test (run "{intersect {union {1 2 3} {7 7 7}} {4 2 3}}") => '(2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}} {union {scalar-mult 3 B} {4 5 7 9 8 8 8}}}") =error> "eval: free identifier:")
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}")
      => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "eval: free identifier:")



;;///////--------------------------------------part B-----------------------------------------///////////

#|
In Part B:
We are required to use WAE language:
In order to check if there are unprotected organs in the language and return a list of those organs.
|#

(define-type WAE 
  [Num Number] 
  [Add WAE WAE]
  [Sub WAE WAE]
  [Mul WAE WAE]
  [Div WAE WAE]
  [Id Symbol]
  [With Symbol WAE WAE])

(: parse-sexpr : Sexpr -> WAE) 
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> WAE)
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: subst : WAE Symbol WAE -> WAE)
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]))


(: is-member-symbol? : Symbol (Listof Symbol) -> Boolean) ;; help function: to cheack if the symbol is a member and delete dup.
 (define (is-member-symbol? n l)
    (cond
    [(null? l) #f]
    [(equal? n (first l)) #t]
    [else (is-member-symbol? n (rest l))]))

(: remove-dup-list-of-symbol : (Listof Symbol) -> (Listof Symbol));;help function: that receives a list and returns a list of symbols in the correct order and without duplicates.
 (define (remove-dup-list-of-symbol lstS)
  (cond
   [(null? lstS) null]
   [(is-member-symbol? (first lstS) (rest lstS)) (append (list (first lstS)) (remove-dup-list-of-symbol (delete (rest lstS) (first lstS))))]
   [else (append (list (first lstS)) (remove-dup-list-of-symbol (rest lstS)))]))



(: delete : (Listof Symbol) Symbol  -> (Listof Symbol)) ;;help function: receives a symbol and a list of symbols and checks whether it should be deleted from the list and return the list.

(define (delete ls item) 
    ( cond
    [(null? ls) '()]
    [(pair? (car ls)) (cons (delete (car ls) item) (delete (cdr ls) item))]
    [(equal? (car ls) item) (delete (cdr ls) item)] 
    [else (cons (car ls) (delete (cdr ls) item))]))

#|

The function accepts the language WAE and returns a list of symbols
The function: checks if there are undefined organs in the language if they exist creates their list
If no returns an empty list.
|#


(: freeInstanceList* :  WAE -> (Listof Symbol))
 (define (freeInstanceList* expr*)
  (cases expr*
  [(Num n) '()]
  [(Add l r) (append (freeInstanceList l) (freeInstanceList r))]
  [(Sub l r) (append (freeInstanceList l) (freeInstanceList r))]
  [(Mul l r) (append (freeInstanceList l) (freeInstanceList r))]
  [(Div l r) (append (freeInstanceList l) (freeInstanceList r))]
  [(Id name) (list name)]
  [(With bound-id named-expr bound-body) (freeInstanceList (subst bound-body bound-id named-expr))]))
   
(: freeInstanceList : WAE -> (Listof Symbol))
  (define (freeInstanceList expr)
    (remove-dup-list-of-symbol (freeInstanceList* expr)))


(test (freeInstanceList (parse "w")) => '(w))
(test (freeInstanceList (parse "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (With 'x (Num 2) (Add (Id 'x) (Num 3)))) => '()) 
(test (freeInstanceList (parse "{with {x 2} {with {xx 5} {with {xxx 7} {+ a {+ z b}}}}}")) => '(a z b))
(test (freeInstanceList (With 'x (Num 2) (With 'xx (Num 5) (With 'xxx (Num 7) (Add (Id 'a) (Add (Id 'z) (Id 'b))))))) => '(a z b))
(test (freeInstanceList (parse "{with {x 2} {with {xx 5} {with {xxx 7} {+ a {- z {* a {/ w {+ z b}}}}}}}}")) => '(a z w b))
(test (freeInstanceList (parse "{* {- {/ 100 10} {+ aa p}} {with {x 2} {+ x p}}}")) => '(aa p))
(test (freeInstanceList (parse "{+ z {+ x z}}")) => '(z x))
(test (freeInstanceList (parse "{+ t {+ z {+ z x}}}")) => '(t z x))
(test (freeInstanceList (parse "{with {xxx 2} {with {yyy 3} {+ {- xxx {+ zz yy}} {* z xx}}}}")) => '(zz yy z xx))
(test (freeInstanceList (parse "{with {xxx 2} {with {yyy 3} {with {s t} {+ {- s {+ zz yyy}} {* z xx}}}}}")) => '(t zz z xx))

