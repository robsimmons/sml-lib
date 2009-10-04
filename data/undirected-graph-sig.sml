(* An imperative, undirected, weighted graph. Set of nodes, any pair
   of which can be connected with an edge, which edge can have an
   arbirary weight. *)
signature UNDIRECTEDGRAPH =
sig
    exception UndirectedGraph of string

    (* Supplied by functor argument *)
    type weight

    (* Nodes in this graph, associated with arbitrary data. 
       All operations require each of the nodes to be from
       the same graph. *)
    type 'a node
    val cmp : 'a node * 'a node -> order
    val eq : 'a node * 'a node -> bool

    type 'a graph

    val empty : unit -> 'a graph
    val add : 'a graph -> 'a -> 'a node
    val get : 'a node -> 'a

    (* Create an edge between the two nodes with a weight. 
       Weights must be strictly positive. Implies symmetric connection
       with same weight. Nodes must not already have an edge. *)
    val addedge : 'a node -> 'a node -> weight -> unit

    (* NONE if no edge, otherwise SOME of that weight *)
    val hasedge : 'a node -> 'a node -> weight option

    (* Enumerate all of the edges from a given node. *)
    val edges : 'a node -> ('a node * weight) list

    (* Apply function to all nodes in the graph, in arbitrary order. *)
    val app : ('a node -> unit) -> 'a graph -> unit

    (* shortestpaths src
       Compute a new graph based on the one that the source node is
       contained in. Its data are augmented with the shortest distance
       to/from the node src. Every node in the original graph has a
       node in the destination; a function is returned that allows
       nodes to be promoted. *)
    val shortestpaths : 'a node ->
        { graph : ('a * weight option) graph,
          promote : 'a node -> ('a * weight option) node }

end

signature UNDIRECTEDGRAPHARG =
sig

    type weight
    (* XXX opns on weights *)
    val compare : weight * weight -> order
    val + : weight * weight -> weight
    (* Distance from a node to itself. w + z must equal w. *)
    val zero : weight

end