;; A simple LISP interpreter written by Dr Klefstad for ICS 141 at UCI
;; Of course, I deleted lots of it to let you learn more about evaluation.

;; my-assoc returns the association (binding) of a variable in the association
;; list.  An alist is a list of this form:
;; ((var1 . val1) (var2 . val2) ... (varN . valN))
;; where each vari is a symbol representing a variable (or parameter) name
;; and each vali is the value of the variable.
;; assoc returns the association of a given symbol, e.g,
;; (assoc 'myvar '((a . 10)(b a b c)(myvar d e f)))
;; returns (myvar d e f) and you take the cdr of that to get myvar's value
;; (d e f)
;; We will use alists for the stack of variables and their values.  Assoc
;; always finds the first association of a variable, and this is how we
;; implement dynamic scoping.  New defintions of a variable will hide older
;; definitions, but the older definitions will come back into scope when
;; recursive evaluation unwinds.
;; setq and defun will push a new association on the global-alist.
;; whenever we apply a function, we will bind the formals to the evaluated
;; actuals pushing these new bindings onto the local alist and then
;; evaluate the body of the function in that new scoping context.

;; You need to write this one.
(defun my-assoc (v alist)
  (cond
    ((null v) nil)
    ((eq (caar alist) v) (car alist))
    (t (my-assoc v (cdr alist)))
  )
)

;; This one is done
(defun my-eval (e alist)
    (cond ((atom e) (my-eval-atom e alist))
          (t (my-apply (car e) (cdr e) alist))
    )
)

;; You need to write this one.
(defun my-eval-atom (e alist)
;; how do you evaluate an atom???
;; Remember there are special cases: T, NIL, ASYMBOL, 10, "Hello"
	(cond 	    
		((eq t e) T)
	    ((null e) nil)
		((symbolp e) (cdr (my-assoc e alist)))
		(t e)
	)
)

;; This one is done, but you must write the functions it calls
(defun my-apply (fn args alist)
    (cond ((atom fn) (my-apply-atom fn args alist))
          ( t (my-apply-lambda fn args alist)))
)

;; You need to write this one.
;; Utility function for eval-cond and apply-lambda.  Evaluates each expression
;; in l and returns the value of the last expression
(defun my-eval-list (l alist)
	(cond 
		((null l) nil)
		((null (cdr l)) (my-eval (car l) alist))
		(t (my-eval(car l) alist)(my-eval-list (cdr l) alist))
	)
)

;; You need to write this one.
(defun my-apply-lambda (fn args alist)
;; bind the formals to the evaluated actuals then evaluate the body in that
;; new scoping context (i.e., that becomes the new alist for recursive
;; evaluation of the function body.  Return the value of the last 
;; expression in the body (using eval-list).
	(my-eval-list (cdr fn) (my-bind-formals (car fn) args alist))
)

;; You need to write this one.
(defun my-bind-formals (formals actuals alist)
;; This takes a list of formals and unevaluated actuals.  It should evaluate
;; each actual and bind it to its corresponding formal placing them all on
;; the front of the alist.  It should return the alist with the new bindings
;; on the front.  This will be used to evaluate calls to functions defined
;; via defun.
;; e.g., (my-bind-formals '(a) '((add 1 b)) '((b . 10)))
;; will return ((a . 11) (b . 10))
;; Note there will be one actual parameter for each formal parameter.
    (cond
        ((null formals) alist)
        ((null actuals) alist)
        (t (cons(cons (car formals) (my-eval (car actuals) alist)) (my-bind-formals (cdr formals)(cdr actuals) alist)))
    )
)

;; You need to write this one.  Handle the primitives as special cases, then
;; handle user defined functions (defined via defun) in the default case.
;; These are the only functions we handle: eq, car, cdr, cons, quote, cond,
;; defun, eval, setq, and user defined functions (defined via defun) that
;; we have evaluated.  You can add more built-ins (like plus, times, atom,
;; listp) as you like for testing.
(defun my-apply-atom (fn args alist)
    (cond ((eq fn 'eq)
           (eq (my-eval (car args) alist) (my-eval (cadr args) alist)))
          ;; I wrote the first one, eq, for you, you write the rest
          ((eq fn 'null)
           (null (my-eval(car args) alist)))
          ((eq fn 'car) 
           (car (my-eval (car args) alist)))
          ((eq fn 'cdr)
           (cdr (my-eval (car args) alist)))
          ((eq fn 'cons)
           (cons (my-eval (car args) alist) (my-eval (cadr args) alist)))
          ((eq fn 'quote)
           (car args))
          ((eq fn 'setq) 
           (my-eval-setq (car args) (cadr args)))
          ;; these are (nearly) done, but you must write the sub-functions
          ((eq fn 'cond) (my-eval-cond args alist))
          ((eq fn 'defun) (my-eval-defun args alist))
          ((eq fn 'eval) (my-eval (my-eval (car args) alist) alist))
          (T (my-apply (cdr (my-assoc fn alist));; get the lambda from the alist, 
                       args alist))
    )
)


;; You need to write this one.
(defun my-eval-setq (var val)
;; just push a new association of the var and its evaluated val onto the
;; global alist
	(cdr(my-assoc var (setq global-alist (cons (cons var (my-eval val global-alist)) global-alist))))
)

;; You need to write this one.  You should know how cond works at this point.
(defun my-eval-cond (clauses alist)
	(cond
		((null clauses) nil)
		((eq t (my-eval (caar clauses) alist))(my-eval-list (cdar clauses) alist))
		(t (my-eval-cond (cdr clauses) alist))
	)
)

;; You need to write this one.
(defun my-eval-defun (body alist)
;; just push the function body onto the global alist.  It is already an
;; association, e.g., (equal (L1 L2) (cond (...))) and (assoc 'equal in
;; the global alist will return this.  You can then take the cdr and you
;; have a list containing the formal parameters and the expressions in
;; the function body.
	(caar (setq global-alist (cons body global-alist)))
)

;; This one is done, it just initializes the global alist where global
;; settings, like those defined via setq and defun, go.
(setq global-alist nil)
;; to push a new value, (setq global-alist (cons (cons 'newvar 'newval) global-alist))

;; This one is done, it will become the new top-level for LISP.  After you
;; load this file, call (my-top) and then you can type in expressions and
;; define and call functions to test your my-eval.
(defun my-top ()
    (prog ()
        top (print (my-eval (read) global-alist))
            (terpri) ;; prints a newline
            (go top) ;; loops forever
    )
)