(define replace-split
    (lambda (str rep sub) 
    ; replace-split: a procedure that takes str (type: string) and 
    ; returning a list with all instances of rep is replaced by sub.
    (begin
        ; ls: the internal representation fo the string
        ; being processed. I don't like using let
        ; simplicity is beauty
        (define ls (string->list str))
        ; check for EOL
        (if (eqv? '() (cdr ls))
            ; return the resulting list if EOL is reached
            ls
            (if (eqv? (car ls) rep)
                ; replace the first item with sub
                (cons 
                    sub
                    (replace-split 
                        (list->string (cdr ls))
                        rep
                        sub)
                )
                ; skip char not matching rep
                (cons 
                    (car ls)
                    (replace-split 
                        (list->string (cdr ls))
                        rep
                        sub)
                )
            )
        )
    )   
    )
)
