#lang at-exp racket

(provide (contract-out
          [graph->pict ({(hash/c any/c list?)}
                        {#:layout symbol?}
                        . ->* .
                        pict?)]

          [graph->png! ({(hash/c any/c list?)
                         path-string?}
                        {#:exists symbol?
                         #:layout symbol?}
                        . ->* .
                        any)]
          [graph->png-bytes ({(hash/c any/c list?)}
                             {#:layout symbol?}
                             . ->* .
                             bytes?)]

          [dot->png! ({string?
                       path-string?}
                      {#:exists symbol?
                       #:layout symbol?}
                      . ->* .
                      any)]
          [dot->png-bytes ({string?}
                           {#:layout symbol?}
                           . ->* .
                           bytes?)]

          [graph->dot ((hash/c any/c list?) . -> . string?)]

          [current-dot-graph-settings (parameter/c hash?)]
          [current-dot-preamble (parameter/c string?)]))

(require (only-in racket/draw read-bitmap)
         pict)

(define (dot->png-bytes dot-code
                        [out #f]
                        #:layout [layout-exe 'dot])
  (define dot (find-executable-path (~a layout-exe)))
  (unless (path-string? dot)
    (raise-user-error 'dot->png-bytes
                      @~a{No graphviz executable '@layout-exe' found.}))
  (match-define (list stdout stdin pid stderr ctl)
    (process*/ports out #f #f
                    dot "-Tpng"))
  (displayln dot-code stdin)
  (close-output-port stdin)
  (ctl 'wait)
  (match (ctl 'status)
    ['done-ok
     (close-input-port stderr)
     (unless out
       (begin0 (port->bytes stdout)
         (close-input-port stdout)))]
    [else
     (define error-bytes (port->bytes stderr))
     (unless out (close-input-port stdout))
     (close-input-port stderr)
     (error 'dot->png!
            @~a{
                `dot` failed with stderr:
                @error-bytes
                })]))

(define (dot->png! dot-code
                   path
                   #:exists [exists 'error]
                   #:layout [layout 'dot])
  (call-with-output-file path
    #:exists exists
    (λ (out) (dot->png-bytes dot-code out #:layout layout))))

(define (graph->pict graph #:layout [layout 'dot])
  (read-bitmap (open-input-bytes (graph->png-bytes graph #:layout layout))))

(define (graph->png-bytes graph #:layout [layout 'dot])
  (dot->png-bytes (graph->dot graph) #:layout layout))

(define (graph->png! graph
                     path
                     #:exists [exists 'error]
                     #:layout [layout 'dot])
  (dot->png! (graph->dot graph)
             path
             #:exists exists
             #:layout layout))

(define current-dot-graph-settings (make-parameter (hash)))
(define current-dot-preamble (make-parameter ""))
(define (graph->dot graph)
  @~a{
      digraph @;
      {
       @(current-dot-preamble)
       @(string-join (for/list ([{k v} (in-hash (current-dot-graph-settings))])
                       @~a{@k = @v})
                     ";\n")
       @(graph->dot-edges graph)
       }
      })
(define (graph->dot-edges graph
                          #:edge-styles [stylize-edge (const #f)]
                          #:node-styles [node-styles ""])
  (string-join
   (cons node-styles
         (for*/list ([{src dests} (in-hash graph)]
                     [dest (in-list dests)])
           @~a{
               "@src" -> "@dest"@;
               @(or (stylize-edge src dest)
                    "")
               }))
   "\n"))

#;(define (visualize-graph-cut->png! graph
                                   groups
                                   cut-edges
                                   [path cut-graph.png])
  (define (group->subgraph group name)
    @~a{
        subgraph cluster_@name @;
        {
         @(string-join
           (for/list ([node (in-set group)])
             @~a{"@(defn-name node)"})
           "\n")
         }})
  (dot->png!
   @~a{
       digraph @;
       {
        @(string-join
          (for/list ([group (in-set groups)]
                     [i (in-naturals)])
            (group->subgraph group i))
          "\n")
        @(graph->dot-edges graph
                           (λ (an-edge)
                             (define cut? (set-member? cut-edges an-edge))
                             (and cut? "[style=dotted]")))
        }
       }
   path)
    (bitmap path))

