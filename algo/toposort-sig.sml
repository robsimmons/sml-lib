
(* Topological sort algorithm. 

   Given a list of nodes and constraints between them,
   produce a list that satisfies the constaints, if
   one exists. The constraints are all of the form,
   "item a must appear somewhere before item b in
   the list." If a topological sorting doesn't exist,
   raise the exception TopoSort.
   
   To use this algorithm, map 'node' over your items
   to create nodes. Then, create a list of constraints
   (pairs of nodes that must be ordered), and call
   solve. Finally, use 'get' to retrieve the original
   elements from the node list returned.

*)

signature TOPOSORT =
sig
    
    exception TopoSort of string

    type 'a node

    type 'a constraint

    (* constraint a b => "a must appear before b" *)
    val constraint : 'a node * 'a node -> 'a constraint

    val node : 'a -> 'a node

    val sort : 'a node list -> 'a constraint list -> 'a node list

    val get : 'a node -> 'a

end
