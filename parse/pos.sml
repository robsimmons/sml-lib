(* Chris Okasaki / Robert Harper / Kevin Watkins / Tom Murphy VII
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
*)

structure Pos :> POS =
struct

  type Coord = {file : string, char : int, line : int, abschar : int}

  datatype pos = At of Coord
    | Between of Coord * Coord

  fun initposex s = At {file = s, char = 1, line = 1, abschar = 1}
  val initpos = initposex "?"

  fun nextchar (At {file, char, line, abschar}) = 
    At {file = file, char = char + 1,
        line = line, abschar = abschar + 1}
    | nextchar (Between (_,{file, char, line, abschar})) = 
    At {file = file, char = char + 1,
        line = line, abschar = abschar + 1}

  fun nextline (At {file, char, line, abschar}) = 
    At {file = file, char = 1, line = line + 1,
        abschar = abschar + 1}
    | nextline (Between (_,{file, char, line, abschar})) = 
    At {file = file, char = 1, line = line + 1, abschar = abschar + 1}

  val tabsize = 8
  fun tab {file, char=c, line=l, abschar} = 
    { file = file, 
      char = c + (tabsize - (c-1) mod tabsize),
      line = l,
      abschar = abschar + 1 }

  fun nl {file, char=c, line=l, abschar} = 
    {file = file, char = 1, line = l+1, abschar = abschar + 1}

  fun graph {file, char=c, line=l, 
             abschar} = {file = file, char = c+1, line = l,
                                       abschar = abschar + 1}

  fun advance (coord,#"\n") = nl coord
    | advance (coord,#"\t") = tab coord
    | advance (coord,_) = graph coord

  fun markstreamex f s =
      let
          fun mark (s, coord) () =
              let
                  val (c, s) = Stream.uncons s
                  val coord' = advance(coord,c)
              in
                  Stream.lcons((c,Between(coord,coord')),mark(s,coord'))
              end
              handle Stream.Empty => Stream.empty
      in
          Stream.old_delay(mark(s,{file = f, char=1, line=1, abschar=1}))
      end

  fun markstream s = markstreamex "?" s

  (* doesn't make perfect sense, but whatever... *)
  fun markany s =
    let
      fun mark (s, coord) () =
        let val (c, s) = Stream.uncons s
            val coord' = graph coord
        in Stream.lcons((c, Between(coord, coord')), mark(s, coord'))
        end handle Stream.Empty => Stream.empty
    in
      Stream.old_delay(mark(s, {file = "any?", char=1, line = ~1, abschar=1}))
    end
    
  fun right (At coord) = coord
    | right (Between (_,coord)) = coord

  fun left (At coord) = coord
    | left (Between (coord,_)) = coord

  fun rightedge pos = At (right pos)
    
  (* XX could use abschar for the following *)
  fun leftmost (a as {char=c1, line=l1, file=_, abschar=_},
                b as {char=c2, line=l2, file, abschar=_}) : Coord =
        if      l1 < l2 then a
        else if l2 < l1 then b
        else if c1 < c2 then a
        else (* c2 <= c1 *)  b

  fun rightmost (a as {char=c1, line=l1, file=_, abschar=_},
                 b as {char=c2, line=l2, file, abschar}) : Coord =
        if      l1 > l2 then a
        else if l2 > l1 then b
        else if c1 > c2 then a
        else (* c2 >= c1 *)  b

  fun union (p1,p2) =
        Between (leftmost  (left p1,left p2),rightmost (right p1,right p2))

  fun max (p1,p2) =
        Between (rightmost (left p1,left p2),rightmost (right p1,right p2))

  fun min (p1,p2) =
        Between (leftmost  (left p1,left p2),leftmost  (right p1,right p2))

  fun coord2string {file, char, line, abschar} =
      file ^ ":" ^ Int.toString line ^ "." ^ Int.toString char

  fun toString (At coord) = coord2string coord
    | toString (Between (coord1 as {file=f1, ...},
                         coord2 as {file, char, line, abschar})) =
        if coord1 = coord2 then coord2string coord1
        else if f1 = file
             then coord2string coord1 ^ "-" ^ 
                  Int.toString line ^ "." ^ Int.toString (char - 1)
             else coord2string coord1 ^ "-" ^ 
                  coord2string {file = file, char=char-1, line=line, 
                                abschar=abschar-1}

  fun getabs (At c) = getabs (Between (c, c))
    | getabs (Between ({abschar=a, ...}, {abschar=b, ...})) = (a, b)

  fun getcol (At c) = getcol (Between (c, c))
    | getcol (Between ({char=a, ...}, {char=b, ...})) = (a, b)

end
