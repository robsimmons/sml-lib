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

    (* Compute a minimal spanning tree from a graph which has been
       annotated with the minimum distance from each node to the
       desired root (for example, the output of shortestpaths). May
       fail or produce a nonsense tree if the distances are not
       correct. Otherwise, the output is a tree (acyclic graph) which
       connects all points that are reachable from the root (the
       single node with distance 0) by pointing at the next node on
       the way to the root. Distances are copied from the
       shortestpaths graph.

       If some nodes are unreachable (they have distance NONE) then
       they will have no parent in the output. The root also has
       no parent. *)
    datatype 'a span = S of { a : 'a, 
                              dist : weight option,
                              parent : 'a span node option }
    val spanningtree : ('a * weight option) graph ->
        { graph : 'a span graph,
          promote : ('a * weight option) node -> 'a span node }

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