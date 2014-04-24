#lang racket

(provide Sym Sym-name Sym-ls Sym-dom
         name-sym dom-sym
         sd vocab-master)

(struct Sym (name ls dom) #:transparent)
(define (sd sym n) (list-ref (Sym-ls sym) n))
(define (name-sym s) (symbol->string (Sym-name s)))
(define (dom-sym s) (symbol->string (Sym-dom s)))

(define-syntax define-vocab-list
  (syntax-rules ()
    [(define-vocab-list «vocab-list»
       («word» «sym») ...)
     (begin
       (define «word» (Sym '«word» (dcon «sym») '«vocab-list»)) ...
       (define «vocab-list» (list «word» ...))
       (provide «vocab-list» «word» ...))]))

(define (dconvert dls) (map (λ (dl) (map (λ (d) (equal? d 1)) dl)) dls))
(define (dcon dls)
  (dconvert (map (λ (n) 
                   (let* ([d1 (floor (/ n 1000))]
                          [d2 (floor (/ (- n (* 1000 d1)) 100))]
                          [d3 (floor (/ (- n (* 1000 d1) (* 100 d2)) 10))]
                          [d4 (- n (* 1000 d1) (* 100 d2) (* 10 d3))])
                     (list d1 d2 d3 d4)))
                 dls)))

(define (dback dls)
  (map (λ (ls)
         (+ (if (first ls) 1000 0) (if (second ls) 100 0) 
            (if (third ls) 10 0) (if (fourth ls) 1 0)))
       dls))

(define-vocab-list Narayan
  (nature     '(0110 0010 1011 1001 1100))
  (love       '(1010 1000 0111 0010 1101))
  (force      '(1100 0000 1000 0000 1101))
  (transform  '(1110 1100 1001 0010 0010))
  (change     '(1001 0100 0011 1000 1111))
  (machine    '(1011 1100 1000 0010 0110))
  (future     '(1100 1000 1000 1011 0011))
  (cycle      '(1101 0000 0000 0000 1110))
  (merge      '(1111 1100 0000 0000 1001))
  (dependence '(1001 1001 1000 1111 0001))
  
  (void        '(1110 0001 0000 0000 1110))
  (energy      '(1100 0011 0100 0100 1011))
  (mutual      '(0001 0100 1001 0001 0110))
  (contradict  '(0010 1111 0000 1100 0110))
  (power       '(0100 0011 0110 0000 0000))
  (possibility '(1010 1001 1110 0010 1101))
  (convey      '(1010 0011 0100 0011 0001))
  (encourage   '(0110 1111 0100 0110 0010))
  (wisdom      '(0101 0001 1110 0110 0001))
  (dynamic     '(1110 0011 0000 1100 0110))
  
  (intelligence '(1000 1100 0100 1101 0011))
  (entropy      '(1010 0001 0110 0111 1110))
  (society      '(1111 1100 1001 0011 0110))
  (chaos        '(1010 0001 0101 1100 0010))
  (growth       '(1110 0111 0000 1101 0000))
  (civilization '(1000 1101 1101 0011 0110))
  (spur         '(0100 1111 0100 0110 1000))
  (infinite     '(0110 1111 1000 0000 0001))
  (motion       '(0011 1100 0100 0000 1001))
  (harmony      '(0101 1100 0110 0011 1001))
  
  (resurrect  '(0101 0001 1100 1111 0001))
  (weave      '(0000 1111 1010 0110 0000))
  (rebirth    '(1110 0001 0110 0010 1101))
  (control    '(1010 0011 0110 0100 0001))
  (sacrifice  '(0101 0000 0110 0011 0101))
  (time       '(1101 1111 0000 1100 0001))
  (constraint '(0110 1011 0101 0100 0000))
  (inhibit    '(0000 1001 0011 1001 0011))
  (creativity '(0010 1111 1100 1110 0010))
  (stimulate  '(1100 1110 1100 1101 0000))
  
  (momentum   '(0011 1001 0000 1100 0111))
  (balance    '(1010 0000 0110 0000 1001))
  (resilience '(0110 1001 0000 0000 1111))
  (flow       '(1001 0000 0000 1100 0110))
  (believe    '(0110 1011 0110 0000 0000))
  (tradition  '(1011 1000 1100 0010 0110))
  (nurture    '(1001 0110 1000 1101 0001))
  (honor      '(1010 1100 0110 0000 0010))
  (form       '(1010 0001 1111 0110 1000))
  (question   '(1000 1111 0001 0100 0001))
  
  (static   '(1010 0001 1001 0111 0001))
  (exist    '(1110 1001 0100 0100 1101))
  (elevate  '(0110 1111 0110 0000 0001))
  (survival '(0110 0001 1100 0000 1111))
  (system   '(0000 1110 0010 1011 1000))
  (remember '(1010 1000 0000 0100 1001))
  (sustain  '(0010 1101 0000 0000 1101))
  (ethereal '(1010 0101 1000 0110 1010))
  (discover '(1100 0010 1010 1110 1000))
  (explore  '(0010 1101 1000 1100 0000))
  )

(define-vocab-list Martian
  (Circle '(1111 1111 1111 1111 1111)))

(define vocab-master 
  (apply append 
         (map
          (λ (vlist) 
            (filter 
             (λ (sym) (and (equal? (length (Sym-ls sym)) 5) 
                           (andmap (λ (cir) (equal? (length cir) 4)) (Sym-ls sym))))
             vlist))
          (list Narayan Martian))))
