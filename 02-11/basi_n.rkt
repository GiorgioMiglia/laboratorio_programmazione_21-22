;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname basi_n) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require racket/string)
(define stringa-val ; data una cifra (formato stringa) e una stringa restituisce una sottonstringa che parte dal valore dato
    (lambda (val base) ; val: stringa formata da una cifra, base: stringa formata da cifre ordinate di una base numerica
        (if (string=? (substring base 0 1) val)
            base
            (stringa-val val (substring base 1))
        )
    )
)

(define valore ; data una cifra (formato stringa) e una stringa rappresentante una base restituisce il valore in base 10 della cifra nella base fornita
    (lambda (val base) ; val: stringa formata da una cifra, base: stringa formata da cifre ordinate di una base numerica
        (- ; il valore si ottiene sottraendo la lunghezza della sottostringa descritta sopra alla lunghezza della stringa base
            (string-length base)
            (string-length
                (stringa-val val base)
            )
        )
    )
)

(define b-dec ; converte un numero intero da una base fornita a base 10
    (lambda (base str) ; base: stringa formata da cifre ordinate di una base numerica, str: numero intero (formato stringa)
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

(define parte_intera ; restituisce la parte prima della virgola ( . ) di un numero (formato stringa) togliendo anche eventuale segno (+ o -)
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

(define parte_fraz ; restituisce la parte dopo la virgola ( . ) di un numero (formato stringa)
    (lambda (str)
        (if (string=? (substring str 0 1)".")
            (substring str 1)
            (parte_fraz (substring str 1))
        )  
    )
)

(define rep->number ; dato un numero (formato stringa) e una base converte in base 10
    (lambda (base str) ; base: stringa formata da cifre ordinate di una base numerica, str: numero formato da cifre contenute in base,
                        ; con eventuali segno (+ o -) e virgola ( . )
        (if (string-contains? str ".")
                    ; se un numero da convertire contiene la virgola si può procedere convertendo invece
                    ; il numero ottenuto ignorando la virgola e dividendo poi il risultato per una potenza 
                    ; della base di partenza (es. base 2 -> 2, base H -> 16) con esponente il numero di cifre dopo la virgola
                    ; es. 10.1 in base 2 --> 101 base 2 / 2^1 = 5/2 = 2.5
            (cond
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
            (cond ; se il numero fornito non ha la virgola si converte normalmente, togliendo prima eventuale segno (+ o -)
                ((string=? (substring str 0 1) "+") (b-dec base (substring str 1)))
                ((string=? (substring str 0 1) "-") (* (b-dec base (substring str 1)) -1))
                (else (b-dec base str))
            )
        )
    )
)