;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname basi_n) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require racket/string)
(define stringa-val
    (lambda (val base)
        (if (string=? (substring base 0 1) val)
            base
            (stringa-val val (substring base 1))
        )
    )
)

(define valore
    (lambda (val base)
        (-
            (string-length base)
            (string-length
                (stringa-val val base)
            )
        )
    )
)

(define b-dec
    (lambda (base str)
        (let ((k (- (string-length str) 1)))
            (if (= k 0)
                (valore str base)
                (+ 
                    (* (string-length base) (b-dec base (substring str 0 k)))
                    (valore (substring str k) base)
                )
            )
        )
    )
)

(define parte_intera
    (lambda (str)
        (if (string=? (substring str (- (string-length str) 1))".")
            (if (or (string=? (substring str 0 1) "+") (string=? (substring str 0 1) "-"))
                (substring str 1 (- (string-length str) 1))
                (substring str 0 (- (string-length str) 1))
            )
            (parte_intera (substring str 0 (- (string-length str) 1) ))
        )
    )
)

(define parte_fraz
    (lambda (str)
        (if (string=? (substring str 0 1)".")
            (substring str 1)
            (parte_fraz (substring str 1))
        )  
    )
)

(define rep->number ; conversione stringa formata da 0, 1, +, -, . in numero decimale
    (lambda (base str)
        (if (string-contains? str ".") ; = se la stringa str contiene "."
; per convertire un numero bin con virgola si può anche convertire il numero ottenuto ignorando la virgola 
; e dividendolo successivamente per una potenza di 2 con all'esponente il numero di cifre dopo la virgola 
; es. 10.1 bin = 2.5 dec --> 10.1 bin = 101 bin / 2^1 = 5/2 = 2.5 dec
            (cond ; (exact->inexact) trasforma una frazione in numero con virgola es. 5/2 -> 2.5
                ((string=? (substring str 0 1) "+")
                    (exact->inexact 
                        (/ 
                            (b-dec base (string-append (parte_intera str) (parte_fraz str))) 
                            (expt (string-length base) (string-length (parte_fraz str)))
                        )
                    )
                )
                ((string=? (substring str 0 1) "-")
                    (exact->inexact
                        (* (/ 
                                (b-dec base (string-append (parte_intera str) (parte_fraz str)))
                                (expt (string-length base) (string-length (parte_fraz str)))
                            ) -1)
                    )
                )
                (else (exact->inexact
                            (/
                                (b-dec base (string-append (parte_intera str) (parte_fraz str)))
                                (expt (string-length base) (string-length (parte_fraz str)))
                            )
                        )
                
                )
            )
            (cond ; se nella stringa passata non è presente la virgola si converte normalmente (togliendo solo il + o -)
                ((string=? (substring str 0 1) "+") (b-dec base (substring str 1)))
                ((string=? (substring str 0 1) "-") (* (b-dec base (substring str 1)) -1))
                (else (b-dec base str))
            )
        )
    )
)