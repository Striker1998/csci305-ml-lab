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
fun isMember e Empty = false (*returns false if there is an Empty set*)
|   isMember e (Set(x, xs))= (*if e is equal to the value x, then returns true, otherwise it'll check the value with the remaining input set*)
      if (e = x) then true
      else isMember e xs;

(*Makes a Set of the datatype set from a given list*)
fun list2Set [] = Empty (*if empty list, then the set is Empty*)
|   list2Set (x::nil) = Set(x, Empty) (*if only 1 element of the list, then the last element of the set is x*)
|   list2Set (x::xs) = (*if x is a member of the set, then will ignore the value, else it'll add x to the created set*)
      if isMember x (Set(hd(xs), list2Set(xs))) then list2Set(xs) 
      else Set(x, list2Set(xs));

(*Makes a single Union set from 2 sets*)
fun union Empty Empty = Empty (*If both inputs are empty sets, then it will return Empty*)
|   union Empty yset = yset (*If only 1 input set is an empty set, then it will return the other set*)
|   union xset Empty = xset (*If only 1 input set is an empty set, then it will return the other set*)
|   union (Set(x,xs)) yset = (*If the value x is a member of set yset, then it will not add the value to the new set, otherwise, it will create a new set with the value x and recursively call union with sets xs and yset*)
      if isMember x yset then union xs yset
      else Set(x, union xs yset);

(*Makes a single intersection set from 2 sets*)
fun intersect Empty Empty = Empty
|   intersect Empty yset = Empty
|   intersect xset Empty = Empty
|   intersect (Set(x,xs)) yset = if isMember x yset then Set(x, intersect xs yset) else intersect xs yset;
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
