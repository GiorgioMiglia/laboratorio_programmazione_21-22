(define frase (
        lambda (soggetto verbo complemento)
               (string-append (articola soggetto) " "
                              (coniuga verbo soggetto) " "
                              (articola complemento))
         )
  )


(define articola (
                  lambda(parola)
                       (cond 
                         [(equal? #\o (string-ref parola (- (string-length parola) 1)))
                          (string-append "il " parola)]
                         [(equal? #\i (string-ref parola (- (string-length parola) 1)))
                          (string-append "i " parola)]
                         [else
                          (string-append "l" (string (string-ref parola (- (string-length parola) 1))) " " parola)]
                         )
                   )
  )

(define coniuga (
                 lambda (verbo soggetto)
                        (cond
                          [(equal? #\a (string-ref verbo (- (string-length verbo) 3)))
                           (if(singolare soggetto)
                              (substring verbo 0 (- (string-length verbo) 2))
                              (string-append (substring verbo 0 (- (string-length verbo) 2)) "no")
                              )]
                          [else
                           (if (singolare soggetto)
                               (string-append (substring verbo 0 (- (string-length verbo) 3)) "e")
                               (string-append (substring verbo 0 (- (string-length verbo) 3)) "ono")
                               )]
                          )
                  )
  )


(define singolare (
                   lambda (soggetto)
                    (or
                     (equal? #\o (string-ref soggetto (- (string-length soggetto) 1)))
                     (equal? #\a (string-ref soggetto (- (string-length soggetto) 1)))
                     )
                    )
  )
