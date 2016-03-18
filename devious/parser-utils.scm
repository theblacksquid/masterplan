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

(define list-tail
    (lambda (ls k)
    (begin
        (define limit (length ls))
        (define iterate
            (lambda (ls-chars index seed)
                (if (or (= index limit)
                        (eqv? (cdr ls-chars) '())
                    )
                    seed
                    (cons 
                        (list-ref ls-chars index)
                        (iterate 
                            ls-chars
                            (+ index 1)
                            seed
                        ))))
        )
        (iterate ls k '())
)))

(define tokenize ; Does not work, spits out it's native language of Ancient Vulcan
    (lambda (chars)
    (begin
        (define ls-chars (string->list chars))
        (define range (count2sp ls-chars 1))
        (define iterate
            (lambda (a-list seed)
                (if (eqv? (cdr a-list) '())
                    seed
                    (cons
                        (list->string (list-head a-list range))
                        (iterate (list-tail a-list range) seed)
                    ))))
        (iterate ls-chars '())
)))
