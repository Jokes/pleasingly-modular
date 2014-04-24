#lang racket/gui

(require "pm-vocab.rkt")

(define τ (* 2 3.14159265358979))
(define τ/4 (* τ 1/4))
(define τ/8 (* τ 1/8))

; GLOBAL
(define current-sym (first vocab-master))
(define (setsym! s) (set! current-sym s))

; DRAW
(define (draw-seg dc xx yy r ss)
  (let ([x (- xx r)] [y (- yy r)] [d (* 2 r)] [s (- 4 ss)])
    (send dc draw-arc x y d d (+ τ/8 (* τ/4 s)) (+ τ/8 (* τ/4 (add1 s))))))
(define (draw-circle dc x y r [ls '(#t #t #t #t)])
  (for-each (λ (s) (when (list-ref ls s) (draw-seg dc x y r s))) (build-list 4 values)))
(define (draw-circle2 dc x y r)
  (send dc draw-ellipse (- x r) (- y r) (* r 2) (* r 2)))

(define (draw-sym dc [pcol "black"] [bgcol (make-color 240 240 240)]
                  [sym current-sym])
  (let*-values 
      ([(width height) (send dc get-size)] 
       [(smaller-bound) (if (< width height) width height)]
       [(penstroke) (/ smaller-bound 100)]
       [(x0 y0) (values (/ width 2) (/ height 2))]
       [(r0) (- (/ smaller-bound 2) (* 3 penstroke))]
       [(r1) (/ r0 2)]
       [(rdiff) (* (sqrt 2) 1/2 r1)]
       [(x1 y1) (values (- x0 rdiff) (- y0 rdiff))]
       [(x2 y2) (values (+ x0 rdiff) y1)]
       [(x3 y3) (values x2 (+ y0 rdiff))]
       [(x4 y4) (values x1 y3)])
    (send dc set-background bgcol)
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-brush "black" 'transparent)
    (send dc set-pen pcol penstroke 'solid)
    (draw-circle dc x0 y0 r0 (sd sym 0))
    (draw-circle dc x1 y1 r1 (sd sym 1))
    (draw-circle dc x2 y2 r1 (sd sym 2))
    (draw-circle dc x3 y3 r1 (sd sym 3))
    (draw-circle dc x4 y4 r1 (sd sym 4))
    ))

(define (draw-sym-mask dc)
  (draw-sym dc "black" "white"))

(define (export-sym)
  (let*-values ([(width height) (send (send canvas get-dc) get-size)]
                [(smaller-bound) (if (< width height) width height)]
                [(s-bmp) (make-bitmap smaller-bound smaller-bound)]
                [(s-dc) (new bitmap-dc% [bitmap s-bmp])]
                [(msk-bmp) (make-bitmap smaller-bound smaller-bound #f)]
                [(msk-dc) (new bitmap-dc% [bitmap msk-bmp])]
                [(fin-bmp) (make-bitmap smaller-bound smaller-bound)]
                [(fin-dc) (new bitmap-dc% [bitmap fin-bmp])])
    (draw-sym s-dc)
    (draw-sym-mask msk-dc)
    (send fin-dc erase)
    (send fin-dc draw-bitmap s-bmp 0 0 'solid (make-color 0 0 0) msk-bmp)
    (send fin-bmp save-file
          (string-append
           (name-sym current-sym) "-" (dom-sym current-sym) "-"
           (number->string smaller-bound) ".png") 'png)))

; FRAME
(define frame (new frame% [label "Pleasingly Modular Symbols"] [width 800] [height 600]))
(define canvas-p (new vertical-panel% [parent frame] [alignment '(center center)]))
(define menu-p (new horizontal-panel% [parent canvas-p] [alignment '(center center)]
                    [stretchable-height #f]))

(define vocab-drop
  (new choice% [label "Symbol: "]
       [parent menu-p]
       [callback (λ (c e) (setsym! (list-ref vocab-master (send c get-selection)))
                   (send canvas refresh))]
       [choices (map (λ (s) (name-sym s)) vocab-master)]))
(define export-b 
  (new button% [parent menu-p] [label "Export"] [callback (λ (b e) (export-sym))]))

(define my-canvas%
  (class canvas%
    (super-new)))

(define canvas (new my-canvas% [parent canvas-p] [min-width 100] [min-height 100]
                    [paint-callback
                     (λ (canvas dc)
                       (draw-sym dc))]))

(send frame show #t)
