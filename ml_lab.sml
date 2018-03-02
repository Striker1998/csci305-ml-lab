(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Kyle Webster
* kyle1234webster@gmail.com
*
***************************************************************)
Control.Print.printDepth := 1024; (*allows the visibility of the entirety of the set*)
(* f adds 1 to every integer in an input list*)
fun f [] = [] (* function f with an input of an empty list will return an empty list *)
  | f (x::xs) = (x + 1) :: (f xs) (* the overloaded function f with an input of a list where x is the first number in the list and xs is the rest of the list. The function f will add 1 to x and recursively call f with the input of xs *);

(*datatype makes a new data type with inputs element and elementSet and has 2 types: empty and Set where Set is of types 'element and 'elementSet*)
datatype 'element set =
    Empty | Set of 'element * 'element set;

(*isMember checks if e is a member of the set 'set'*)
fun isMember e Empty = false
|   isMember e (Set(x, xs))=
      if (e = x) then true
      else isMember e xs;

(*Makes a Set of the datatype set from a given list*)
fun list2Set [] = Empty
|   list2Set (x::nil) = Set(x, Empty)
|   list2Set (x::xs) = if isMember x (Set(hd(xs), list2Set(xs))) then list2Set(xs) else Set(x, list2Set(xs));


(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
