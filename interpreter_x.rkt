#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a X value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

(define (mupllist->racketlist m-list)
  (if (aunit? m-list)
       null
       (cons (apair-e1 m-list) (mupllist->racketlist (apair-e2 m-list)))))

(define (racketlist->mupllist r-list)
  (if (null? r-list)
       (aunit)
       (apair (car r-list) (racketlist->mupllist (cdr r-list)))))

;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; DO add more cases for other kinds of X expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "X addition applied to non-number")))]
        [(int? e) e]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(ifgreater? e) 
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "X ifgreater applied to non-number")))]
        [(fst? e) 
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "X fst was not applied to apair")))]
        [(snd? e) 
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "X snd was not applied to apair")))]

        [(fun? e) (closure env e)]

        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
               [new-env (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) new-env)
           )]

        [(closure? e) e]
        
        [(call? e)
         (let ([clsr (eval-under-env (call-funexp e) env)])
           (if (closure? clsr)
               (let*([param-val (eval-under-env (call-actual e) env)]
                     [funct (closure-fun clsr)]
                     [param-name (fun-formal funct)]
                     [fun-name (fun-nameopt funct)]
                     [fun-env (cons (cons param-name param-val) (closure-env clsr))])
                 (if fun-name
                     (let ([fun-env (cons (cons fun-name clsr) fun-env)])
                       (eval-under-env (fun-body (closure-fun clsr)) fun-env))
                     (eval-under-env (fun-body (closure-fun clsr)) fun-env)))
               (error "X call was not applied to closure")))]
  
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp e)
  (eval-under-env e null))
        
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

(define mupl-map
  (fun #f "mapfn"
       (fun "mapper" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "mapfn") (fst (var "lst"))) (call (var "mapper") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))
