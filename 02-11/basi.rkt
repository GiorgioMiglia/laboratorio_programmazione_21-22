;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname basi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require racket/string) ;questo serve per il comando a riga 56
(define parte_intera ; data una stringa formata da (0, 1, +, -, .) ritorna la parte prima della virgola ( . ) togliendo anche eventuale segno (+ o -)
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

(define parte_fraz ; data una stringa formata da (0, 1, +, -, .) ritorna la parte dopo la virgola ( . )
    (lambda (str)
        (if (string=? (substring str 0 1)".")
            (substring str 1)
            (parte_fraz (substring str 1))
        )  
    )
)

; conversione numero intero da bin a dec ---------------

(define val
	(lambda (str)
		(if (string=? str "0") 
            0
            1
        )
	)
)

(define bin-dec
    (lambda (str)
        (let ((k (- (string-length str) 1)))
            (if (= k 0)
                (val str)
                (+ 
                    (* 2 (bin-dec (substring str 0 k)))
                    (val (substring str k))
                )
            )
        )
    )
)

; -------------------------------------------------------------

(define bin-rep->number ; conversione stringa formata da 0, 1, +, -, . in numero decimale
    (lambda (str)
        (if (string-contains? str ".") ; = se la stringa str contiene "."
; per convertire un numero bin con virgola si può anche convertire il numero ottenuto ignorando la virgola 
; e dividendolo successivamente per una potenza di 2 con all'esponente il numero di cifre dopo la virgola 
; es. 10.1 bin = 2.5 dec --> 10.1 bin = 101 bin / 2^1 = 5/2 = 2.5 dec
            (cond ; (exact->inexact) trasforma una frazione in numero con virgola es. 5/2 -> 2.5
                ((string=? (substring str 0 1) "+") (exact->inexact(/ (bin-dec (string-append (parte_intera str) (parte_fraz str))) (expt 2 (string-length (parte_fraz str))))))
                ((string=? (substring str 0 1) "-") (exact->inexact(* (/ (bin-dec (string-append (parte_intera str) (parte_fraz str))) (expt 2 (string-length (parte_fraz str)))) -1)))
                (else (exact->inexact(/ (bin-dec (string-append (parte_intera str) (parte_fraz str))) (expt 2 (string-length (parte_fraz str))))))
            )
            (cond ; se nella stringa passata non è presente la virgola si converte normalmente (togliendo solo il + o -)
                ((string=? (substring str 0 1) "+") (bin-dec (substring str 1)))
                ((string=? (substring str 0 1) "-") (* (bin-dec (substring str 1)) -1))
                (else (bin-dec str))
            )
        )
    )
)
