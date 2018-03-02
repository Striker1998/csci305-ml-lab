(* full breakdown of quicksort function
*fun partition (pivot, nil) = (nil,nil)
 *| partition (pivot, first :: others) =
  *  let
  *    val (smalls, bigs) = (pivot, others)
  *  in
  *    if first < pivot then (first::smalls, bigs)
  *    else (smalls, first::bigs)
  *  end;

*fun qsort nil = nil
*|   qsort [singleton] = [singleton]
*|   qsort (first::rest) =
*      let
*        val (smalls, bigs) = partition(first,rest)
*      in
*        qsort(smalls) @ [first] @ qsort(bigs)
*      end;
*)

(*fun quicksort [] = []
*  | quicksort (x::xs) =
*    let
*      val (left, right) = List.partition (fn y => y < x) xs
*    in
*      quicksort left @ [x] @ quicksort right
*    end;
*
*fun icmp(a,b) = a < b;*)

fun ordList [] = []
|   ordList list = map (fn x => Char.ord x) list;

ordList [#"A", #"b", #"C"];

fun sqsum (x:: nil) = x*x
|   sqsum (x::xs) = (x*x) + sqsum xs;

sqsum [1,2,3,4];

map (op +) [(1, 2), (3, 4), (5, 6)];
val f = map (op +);
