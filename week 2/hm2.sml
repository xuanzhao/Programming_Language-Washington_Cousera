(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* Problem 1.a *)
fun all_except_option (str, str_lst) =
    case str_lst of
        [] => NONE
      | head::tail => if same_string(head, str)
                      then SOME tail
                      else case all_except_option(str, tail) of
                          NONE => NONE
                        | SOME xl => SOME (head::xl)

(* Problem 1.b *)
fun get_substitutions1 (str_lst_lst, str) = 
    case str_lst_lst of
        [] => []
      | head::tail => case all_except_option(str, head) of 
                        NONE => get_substitutions1(tail, str)
                      | SOME xl => xl @ get_substitutions1(tail, str)

(* Problem 1.c *)   
fun get_substitutions2 (str_lst_lst, str) = 
    let
        fun iter (str_lst_lst, acc) = 
            case str_lst_lst of 
                [] => acc
              | head::tail => case all_except_option(str, head) of 
                                NONE => iter(tail, acc)
                              | SOME xl => iter(tail, xl @ acc) 
    in
        iter (str_lst_lst, [])
    end         

(* Problem 1.d *)
fun similar_names (str_lst_lst, str_lst) =
    let 
      val {first = x, middle = y, last = z} = str_lst
      fun generate_name sub_lst = 
          case sub_lst of
            [] => []
          | head::tail => {first = head, middle = y, last = z}::generate_name tail
    in
      str_lst :: generate_name(get_substitutions2(str_lst_lst, x))
    end 

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* Problem 2.a *)
fun card_color (s, r) = 
    case s of 
      Clubs => Black
    | Spades => Black
    | Diamonds => Red
    | Hearts => Red

(* Problem 2.b *)
fun card_value (s, r) = 
    case r of 
      Num n => n
    | Ace => 11
    | _ => 10

(* Problem 2.c *)
fun remove_card (cs: card list, c: card, e) = 
    case cs of 
      [] => raise e
    | head::tail => if head = c
                    then tail
                    else head :: remove_card (tail, c, e)

(* Problem 2.d *)
fun all_same_color (cs) = 
    case cs of
      [] => true
    | _::[] => true
    | head::mid::tail => card_color(head) = card_color(mid) andalso
                          all_same_color(mid :: tail)

(* Problem 2.e *)
fun sum_cards (cs) = 
    let 
        fun iter(cs, acc) = 
            case cs of 
              [] => acc
            | head::tail => iter(tail, acc + card_value(head))
    in 
      iter(cs, 0)
    end

(* Problem 2.f *) 
fun calc_pre_score (sum, goal) =
    if sum > goal
    then (sum - goal) * 3
    else goal - sum

fun score (cs, goal) = 
    let 
        val sum = sum_cards(cs)
    in
        if all_same_color(cs)
        then calc_pre_score(sum, goal) div 2
        else calc_pre_score(sum, goal)
    end

(* Problem 2.g *)
fun officiate (cs, moves, goal) = 
    let 
        fun play (cs, holds, moves) =
            case moves of 
              [] => score(holds, goal)
             | (Discard card)::tail => play (cs, remove_card(holds, card, IllegalMove), tail)
             | (Draw)::tail => case cs of
                                  [] => score(holds, goal)
                                | car::cdr => if sum_cards(car::holds) > goal
                                              then score(car::holds, goal)
                                              else play(cdr, car::holds, tail)
    in
        play (cs, [], moves)
    end

(*challenge problems*)
(*helper function for 3-a*)
fun sum_cards_challenge (cs) =
    let fun aux (clst, acc, ace_counter) =
            case clst of
                [] => (acc, ace_counter)
              | head::tail => if card_value(head) = 11
                              then aux(tail, acc + 11, ace_counter + 1)
                              else aux(tail, acc + card_value(head), ace_counter)
        fun possible_sums (sum, num_of_aces) =
            if num_of_aces = 0
            then [sum]
            else sum::possible_sums(sum - 10, num_of_aces - 1)
    in
        possible_sums(aux(cs, 0, 0))
    end

(*helper function for 3-a*)
fun lst_min (lst) =
    (*assume lst is not empty*)
    case lst of
        [] => raise List.Empty
      | x::[] => x
      | head::tail => Int.min(head, lst_min(tail))

(*3-a*)
fun score_challenge (cs, goal) =
    let val sum_lst = sum_cards_challenge(cs)
        fun pre_score_lst (lst) =
            case lst of
                [] => []
              | head::tail => calc_pre_score (head, goal)::pre_score_lst(tail)
    in
        if all_same_color(cs)
        then lst_min(pre_score_lst(sum_lst)) div 2
        else lst_min(pre_score_lst(sum_lst))
    end

(*3-a*)
fun officiate_challenge (clst, mlst, goal) =
    let fun aux(card_lst, move_lst, held_cards) =
            case move_lst of
                [] => score_challenge(held_cards, goal)
              | (Discard card)::tail =>
                aux(card_lst, tail, remove_card(held_cards, card, IllegalMove))
              | Draw::tail =>
                case card_lst of
                    [] => score_challenge(held_cards, goal)
                  | first::rest =>
                    if lst_min(sum_cards_challenge(first::held_cards)) > goal
                    then score_challenge(first::held_cards, goal)
                    else aux(rest, tail, first::held_cards)
    in
        aux(clst, mlst, [])
    end

(*helper function for 3-b*)
fun if_discard_then_draw (held, next_card, goal) =
    (*decide whether some card in held can be discarded and then draw the next card
so that a score of 0 can be achieved, if so return SOME with the card to discard
, otherwise return NONE*)
    case held of
        [] => NONE
      | head::tail => if score(next_card::tail, goal) = 0
                      then SOME head
                      else if_discard_then_draw(tail, next_card, goal)

(*3-b*)
fun careful_player (clst, goal) =
    let fun aux (clst, goal, mlst, held) =
            if goal > (10 + sum_cards(held))
            then case clst of
                     [] => mlst@[Draw]
                   | head::tail => if score(head::held, goal) = 0
                                   then mlst@[Draw]
                                   else aux(tail, goal, mlst@[Draw], head::held)
            else if goal = sum_cards(held)
            then mlst
            else case clst of
                     [] => mlst
                   | head::tail => if sum_cards(head::held) > goal
                                   then case if_discard_then_draw(held, head, goal) of
                                            NONE => mlst
                                          | SOME card => mlst@[Discard(card), Draw]
                                   else aux(tail, goal, mlst@[Draw], head::held)
    in
        aux(clst, goal, [], [])
    end
























