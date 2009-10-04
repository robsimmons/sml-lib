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
    (* Create an edge between the two nodes with the same weight.
       Implies symmetric connection with same weight. Nodes must
       not already have an edge. *)
    val addedge : 'a node -> 'a node -> weight -> unit

    (* NONE if no edge, otherwise SOME of that weight *)
    val hasedge : 'a node -> 'a node -> weight option

    (* Enumerate all of the edges from a given node. *)
    val edges : 'a node -> ('a node * weight) list

    (* Apply function to all nodes in the graph, in arbitrary order. *)
    val app : ('a node -> unit) -> 'a graph -> unit

end

signature UNDIRECTEDGRAPHARG =
sig

    type weight
    (* XXX opns on weights *)

end