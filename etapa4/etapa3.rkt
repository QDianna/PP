#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
; din implementarea better-match-exists? - logodnele din listă au pe prima poziție persoana de același gen cu p2
; => prima pozitie = femeie = p2
; => a2-a pozitie = barbat = p1

(define (get-unstable-couples engagements mpref wpref)
  (let unstable-couples ((e engagements)
                         (uc '()))
    (if (null? e)
        uc
        (let* ((couple (car e))
               (f (car couple))
               (m (cdr couple)))
          (if (or (better-match-exists? m f (get-pref-list mpref m) wpref engagements) (better-match-exists? f m (get-pref-list wpref f) mpref (map (λ (pair) (cons (cdr pair) (car pair))) engagements)))
              (unstable-couples (cdr e) (cons couple uc))
              (unstable-couples (cdr e) uc))))))

              
; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let make-couple ((free-men-list free-men)
                    (engagements-list engagements))
    (if (null? free-men-list)
        engagements-list
        (let* ((free-man (car free-men-list)))
          (let find-wife ((free-man-pref (get-pref-list mpref free-man)))
            (let* ((woman (car free-man-pref))
                   (woman-partner (get-partner engagements-list woman))
                   (woman-pref (get-pref-list wpref woman)))
              (if (equal? #f woman-partner)
                  (make-couple (cdr free-men-list) (cons (cons woman free-man) engagements-list))
                  (if (preferable? woman-pref free-man woman-partner)
                      (make-couple (cons woman-partner (cdr free-men-list)) (update-engagements engagements-list woman free-man))
                      (find-wife (cdr free-man-pref))))))))))



; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (apply append (map (lambda (pair) (list (car pair) (cdr pair))) pair-list)))

