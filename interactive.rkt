#lang at-exp racket

(provide (contract-out
          [visualize-graph-interactively!
           ((hash/c any/c list?) . -> . any)]))

(require math/statistics)

(define (visualize-graph-interactively! graph)
  (define html (make-temporary-file "~a.html"))
  (display-to-file (generate-d3-html graph) html
                   #:exists 'truncate)
  (system @~a{firefox '@html'})
  (displayln "Hit enter when done...")
  (read-line)
  (delete-file html))

;; Thanks to https://gist.github.com/lvngd/da9438209e95ddb7e04844dda1ea96f0
;; for getting labels to actually work

(define (generate-d3-html graph)
  (define graph-json (graph->json graph))
  (define width (+ 300 (* 25 (length (remove-duplicates (flatten (hash-values graph)))))))
  (define height (* width 2/3))
  @~a{
  <!DOCTYPE html>
  <meta charset="utf-8">
  <style>

  .links line {
    stroke: #999;
    stroke-opacity: 0.6;
  }

  .nodes circle {
    stroke: #fff;
    stroke-width: 1.5px;
  }

  text {
    font-family: sans-serif;
    font-size: 10px;
  }

  </style>
  <svg width="@~r[width]" height="@~r[height]"></svg>
  <script src="https://d3js.org/d3.v4.min.js"></script>
  <script>

  var svg = d3.select("svg"),
      width = +svg.attr("width"),
      height = +svg.attr("height"),
      radius = 20;

  var color = d3.scaleOrdinal(d3.schemeCategory20)

  var graph = @graph-json;

  @; var node = svg.append("g")
  @; .attr("class", "nodes")
  @; .selectAll("circle")
  @; .data(graph.nodes)
  @; .enter()
  @; .append("circle")
  @; .attr("r", 20)
  @; .attr("cx", function(x){return Math.random()*width})
  @; .attr("cy", function(x){return Math.random()*height})
  @; .attr("fill", function(node){return color(1)})
  @; .call(d3.drag()
  @;   .on("start", dragstarted)
  @;   .on("drag", dragged)
  @;   .on("end", dragended))
  @; .on("click", function(node){console.log(node.id)})

  var link = svg.append("g")
  .attr("class", "links")
  .selectAll("line")
  .data(graph.links)
  .enter()
  .append("line")
  .attr("x1", function(x){return Math.random()*width})
  .attr("y1", function(x){return Math.random()*height})
  .attr("x2", function(x){return Math.random()*width})
  .attr("y2", function(x){return Math.random()*height})
  .attr("stroke-width", function(d){return d.value})


  var node = svg.append("g")
  .attr("class", "nodes")
  .selectAll("circle")
  .data(graph.nodes)
  .enter()
  .append("circle")
  .attr("r", radius)
  .attr("cx", function(x){return Math.random()*width})
  .attr("cy", function(x){return Math.random()*height})
  .attr("fill", function(node){return "#99e6ff"})
  .call(d3.drag()
    .on("start", dragstarted)
    .on("drag", dragged)
    .on("end", dragended))
  .on("click", function(node){console.log(node.id)})

  function truncate(str, len){
                              if (str.length - 4 > len)
                              {
                               return "..." + str.slice(str.length - 4 - len, str.length - 4)
                               }
                              else {
                                    return str.slice(0, str.length - 4)
                                    }
                         }

  var label = svg.append("g")
            .attr("class", "labels")
            .selectAll("text")
            .data(graph.nodes)
            .enter().append("text")
            .text(function(d) { return truncate(d.id, 10); })
            .attr("class", "label");

  label.style("text-anchor", "middle")
      .style("font-size", d => "20px");

  label.call(d3.drag()
            .on("start", dragstarted)
            .on("drag", dragged)
            .on("end", dragended))

  label.on("click", function(node){console.log(node.id)})

  function dragstarted(d){
  if (!d3.event.active) simulation.alphaTarget(0.3).restart();
  d.fx = d.x
  d.fy = d.y
  }
  function dragended(d){
  if (!d3.event.active) simulation.alphaTarget(0);
  d.fx = null
  d.fy = null
  }
  function dragged(d){
  d.fx = d3.event.x
  d.fy = d3.event.y
  }

  var simulation = d3.forceSimulation()
  .force("link", d3.forceLink().id(function(d){return d.id}))
  .force("charge", d3.forceManyBody()
                   .strength(-1000)
                   @;.theta(0.5)
                   .distanceMax(1500))
  .force("center", d3.forceCenter(width/2, height/2))

  simulation.nodes(graph.nodes)
  .on("tick", ticked)

  simulation.force("link")
  .links(graph.links)

  function ticked(){
  @;node.attr("cx", function(d){return d.x})
  @;.attr("cy", function(d){return d.y})
  node.attr("cx", function(d){return d.x = Math.max(radius, Math.min(width - radius, d.x));})
  .attr("cy", function(d){return d.y = Math.max(radius, Math.min(height - radius, d.y));})


  label.attr("x", function(d){return d.x})
  .attr("y", function(d){return d.y})

  link.attr("x1", function(d){return d.source.x})
  .attr("y1", function(d){return d.source.y})
  .attr("x2", function(d){return d.target.x})
  .attr("y2", function(d){return d.target.y})
  }

  </script>

  })


(define (graph->json graph)
  @~a{
      {
       "nodes": [@(string-join (map (λ (v) @~a{{"id": "@v"}})
                                    (remove-duplicates
                                     (append (hash-keys graph)
                                             (append* (hash-values graph)))))
                               ",\n")],
       "links": [@(string-join (for*/list ([{source targets} (in-hash graph)]
                                           [{target count} (in-hash (samples->hash targets))])
                                 @~a{{"source": "@source", "target": "@target", "value": @count}})
                               ",\n")]
       }
      })
