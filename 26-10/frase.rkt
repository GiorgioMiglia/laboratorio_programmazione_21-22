;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Es26-10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define finale
    (lambda (nome)
        (substring nome (- (string-length nome) 1))
    )
)


(define articolo
    (lambda (nome)
        (cond
            ((string=? (finale nome) "o") (string-append "il " nome))
            ((string=? (finale nome) "a") (string-append "la " nome))
            ((string=? (finale nome) "i") (string-append "i " nome))
            ((string=? (finale nome) "e") (string-append "le " nome))
        )
    )
)

(define plurale?
    (lambda (nome)
        (cond
            ((or (string=? (finale nome) "a") (string=? (finale nome) "o")) false)
            ((or (string=? (finale nome) "e") (string=? (finale nome) "i")) true)
        )
    )
)

(define declinazione1?
    (lambda (infinito)
        (let ((desinenza (substring infinito (- (string-length infinito) 3))))
            (if (string=? desinenza "are")
                true
                false
            )
        )
    )
)


(define declina
    (lambda (verbo soggetto)
        (if (plurale? soggetto)
            (if (declinazione1? verbo)
                (string-append (substring verbo 0 (-(string-length verbo) 3)) "ano")
                (string-append (substring verbo 0 (-(string-length verbo) 3)) "ono")
            )
            (if (declinazione1? verbo)
                (string-append (substring verbo 0 (-(string-length verbo) 3)) "a")
                (string-append (substring verbo 0 (-(string-length verbo) 3)) "e")
            )
            
        )
    )
)

(define frase
    (lambda (soggetto verbo complemento)
        (string-append (articolo soggetto) " " (declina verbo soggetto) " " (articolo complemento))
    )
)
