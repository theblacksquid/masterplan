(define replace-split ; Rework in progress, do not use
    (lambda (str rep sub) 
    (begin
        ; ls: the internal representation fo the string
        ; being processed. I don't like using let
        ; simplicity is beauty
        (define ls (string->list str))
        ; check for EOL
        (if (eqv? '() (cdr ls))
            ; return the resulting list if EOL is reached
            ls
            (cond  
                ((eqv? (car ls) #\space) 
                    (replace-split (list->string (cdr ls)) rep sub) 
                )
                ((eqv? (car ls) rep) 
                    (cons 
                        sub
                        (replace-split (list->string (cdr ls)) rep sub)
                    )
                )
                (else 
                    (cons 
                        (car ls)
                        (replace-split (list->string (cdr ls)) rep sub)
                    )
                )
            )
        )
    )
    )   
);

(define count2sp
    (lambda (ls seed)
        (if (or (eqv? (car (cdr ls)) #\space) (eqv? (cdr ls) '())) 
            seed 
            (count2sp (cdr ls) (+ 1 seed))
        )
    )
)


(define list-head
	(lambda (ls k)
	(begin 
		(define iterate
			(lambda (ls-chars index seed)
				(if (or (= index 0)
						(eqv? (cdr ls-chars) '())
					)
					seed
					(cons 
						(car ls-chars)
						(iterate
							(cdr ls-chars)
							(- index 1)
							seed
		)))))
		(iterate ls k '())
)))
; some comment
