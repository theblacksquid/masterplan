(define replace-split
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
)

