(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
         | Variable of string
         | UnitP
         | ConstP of int
         | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
          | Unit
          | Tuple of valu list
          | Constructor of string * valu

fun g f1 f2 p =
    let 
    val r = g f1 f2 
    in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string

(**** you can put all your code here ****)


(* Problem 1 *)
fun only_capitals str_list = 
    List.filter(fn str => Char.isUpper(String.sub(str, 0))) str_list

(* Problem 2 *)
fun longest_string1 str_lst = 
    foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" str_lst

(* Problem 3 *)
fun longest_string2 str_lst = 
    foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" str_lst

(* Problem 4 *)
fun longest_string_helper f =
    foldl (fn (x, y) => if f(String.size(x), String.size(y)) then x else y) ""

val longest_string3 = longest_string_helper op>

val longest_string4 = longest_string_helper op>=

(* Problem 5 *)
val longest_capitalized = 
    longest_string3 o only_capitals

(*
fun longest_capitalized str_lst = longest_string3 (only_capitals str_lst) 
*)

(* Problem 6 *)
fun rev_string str = 
    String.implode (rev (String.explode str))

(* fun rev_string str = (String.implode o rev o String.explode) str *)
(* val rev_string = String.implode o rev o String.explode *)

(* Problem 7 *)
fun first_answer f lst = 
    case lst of 
        [] => raise NoAnswer
      | head::tail => case f(head) of
                        SOME v => v
                      | NONE   => first_answer f tail

(* Problem 8 *)
fun all_answers f lst = 
    let 
        fun iter (lst, acc) = 
            case lst of 
                [] => SOME acc
              | head::tail => case f(head) of
                                SOME v => iter(tail, acc @ v) 
                              | NONE => NONE
    in 
        iter(lst, [])
    end


(* Problem 9 *)
fun count_wildcards pat = g (fn () => 1) (fn str => 0) pat

(* val count_wildcards = g (fn () => 1) (fn str => 0) *)

fun count_wild_and_variable_lengths pat = 
    g (fn () => 1) String.size pat

fun count_some_var (str, pat) =
    g (fn () => 0) (fn x => if x = str then 1 else 0) pat


(* Problem 10 *)
fun check_pat pat = 
    let 
        fun get_var_lst pat = 
            case pat of 
                Variable x => [x]
              | TupleP tps => List.foldl (fn (rest, cur) => cur @ (get_var_lst rest)) [] tps
              | ConstructorP (s, p) => get_var_lst p
              | _ => []

        fun is_dup str_lst =
            case str_lst of
                [] => true
              | head::tail => not (List.exists (fn x => x = head) tail)
                                andalso (is_dup tail)

    in 
        is_dup (get_var_lst pat)
    end


(* Problem 11 *)
fun match (va, pat) =
    case (va, pat) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i = j then SOME [] else NONE
      | (Tuple vs, TupleP tps) => if List.length vs = List.length tps
                                  then all_answers match (ListPair.zip(vs, tps))
                                  else NONE
      | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2 
                                                     then match (v, p)
                                                     else NONE
      | (_, _) => NONE


(* Problem 12 *)
fun first_match va pat_lst = 
    SOME (first_answer (fn x => match(va, x)) pat_lst) 
    handle NoAnswer => NONE
























