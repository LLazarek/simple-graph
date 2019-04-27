#lang racket/base

;; simple-graph
;; ============
;; A small package to interactively render a graph (not in the sense of plot).
;; Original code from Spencer Florence.

(require racket/gui
         mrlib/graph
	 framework
         racket/contract)

(provide (contract-out
          [show-graph
           (->* [(hash/c any/c (listof (or/c labeled? any/c)))]
                [(any/c . -> . string?)]
                void?)]
          [labeled (any/c string? . -> . any)]))

(struct labeled (vertex label) #:transparent)

;; Give `show-graph` a hash mapping vertices to neighbors and the
;; graph will be rendered with nodes converted to text via `~a`
;;
;; In addition to providing plain vertices in the list of links from a
;; given vertex, can provide `labeled` instances to label the edge to
;; that vertex.
(define (show-graph graph-hash [node->string ~a])
  (define f (new (frame:basic-mixin frame%)
		 [label ""]
		 [min-width 800]
		 [min-height 600]))
  (add-graph-from-mapping (send f get-area-container)
                          graph-hash
                          node->string)
  (send f show #t))

(define (add-graph-from-mapping area-container graph [node->string ~a])
  (define/match (node->vertex n)
    [{(labeled v _)} v]
    [{plain} plain])
  (define terms
    (sort
     (set->list
      (for*/set ([(c fs) (in-hash graph)]
                 [t (in-list (cons c fs))])
	(node->vertex t)))
     string<?
     #:key node->string
     #:cache-keys? #t))
  (define-values (snips snips-ordering)
    (for/fold ([h (hash)]
	       [ordering empty])

	      ([c (in-list terms)])
      (define t (new text%))
      (send t insert
	    (node->string c)
	    0)
      (values
       (hash-set
	h
	c
	(new graph-editor-snip%
	     [editor t]))
       (cons c ordering))))
  (define (add-links* map)
    (for* ([(c links) (in-hash map)]
	   [l (in-list links)])
      (define color "blue")
      (define-values [target label]
        (match l
          [(labeled v label) (values v label)]
          [plain (values plain "")]))
      (add-links (hash-ref snips c)
		 (hash-ref snips target)
		 (send the-pen-list find-or-create-pen color 0 'solid)
		 (send the-pen-list find-or-create-pen color 0 'solid)
		 (send the-brush-list find-or-create-brush color 'solid)
		 (send the-brush-list find-or-create-brush color 'solid)
		 label)))
  (add-links* graph)
  (define p (new (graph-pasteboard-mixin pasteboard%)))
  (define ec
    (new editor-canvas%
	 [parent area-container]
	 [editor p]))
  (send p begin-edit-sequence)
  (send p set-draw-arrow-heads? #t)
  (for ([k (in-list snips-ordering)])
    (define s (hash-ref snips k))
    (send p insert s)
    (send s set-margin 15 15 15 15))
  (dot-positioning p)
  (for ([(_ s) (in-hash snips)])
    (send s set-margin 5 5 5 5))
  (send p end-edit-sequence))

(define graph-editor-snip% (graph-snip-mixin editor-snip%))
(define graph-pasteboard% (graph-pasteboard-mixin pasteboard%))



;; Code from Spencer to adjust font size

;; (define menu-bar (new menu-bar% [parent the-frame]))
;;    (define edit-menu (new menu% [label "&Edit"] [parent menu-bar]))
;;    (let ()
;;    (define (font-adjust adj label key shortcut)
;;    (define (adj-font _1 _2)
;;    (editor:set-current-preferred-font-size
;;    (adj
;;    (editor:get-current-preferred-font-size))))
;;    (define (on-demand item)
;;    (define lab
;;    (format
;;    label
;;    (adj
;;    (editor:get-current-preferred-font-size))))
;;    (send item set-label lab)
;;    (send item enable (<= 1 (adj (editor:get-current-preferred-font-size)) 255
;;    )))
;;    (define item
;;    (new menu:can-restore-menu-item%
;;    (shortcut shortcut)
;;    (label "")
;;    (parent edit-menu)
;;    (callback adj-font)
;;    (demand-callback on-demand)))
;;    (void)
;;    #;(send frame set-show-menu-sort-key item key))
;;    (font-adjust add1 "Increase Font Size to ~a" -2 #\=)
;;    (font-adjust sub1 "Decrease Font Size to ~a" -3 #\-))
