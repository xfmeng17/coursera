fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (inputString, identifyStringList) =
    let
	fun recurse_with_two_lists (currentStringList, pastStringList) = 
	    case currentStringList of
		[] => NONE
	      | x :: x' => if same_string(inputString, x)
			   then SOME (pastStringList @ x')
			   else recurse_with_two_lists(x', x :: pastStringList)
    in
	recurse_with_two_lists (identifyStringList, [])
    end

fun get_substitutions1 (substitutionStringListList, str) = 
    case substitutionStringListList of
	[] => []
      | x :: x' => case all_except_option (str, x) of 
		       NONE => get_substitutions1 (x', str)
		     | SOME i  => i @ get_substitutions1 (x', str)

fun get_substitutions2 (substitution, str) = 
    let
	fun get_substitutions_sub (currentStringList, pastStringList) = 
	    case currentStringList of
		[] => pastStringList
	      | x :: x' => case all_except_option (str, x) of
			       NONE => get_substitutions_sub (x', pastStringList)
			     | SOME i => get_substitutions_sub (x', i @ pastStringList)
    in
	get_substitutions_sub (substitution, [])
    end

fun similar_names (substitutions, {first = firstname, middle = middlename, last = lastname}) =
    let
	val subFirstNames = get_substitutions2 (substitutions, firstname)
    in
	let
	    fun substituteNames (subFirstNameList, allNamesList) =
		case subFirstNameList of
		    [] => allNamesList
		  | x :: x' => substituteNames (x', {first = x, middle = middlename, last = lastname} :: allNamesList)
	in
	    substituteNames (subFirstNames, [{first = firstname, middle = middlename, last = lastname}])
	end
    end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (color, _) =
    case color of
	Clubs => Black
      | Diamonds => Red
      | Hearts => Red
      | Spades => Black

fun card_value (_, value) =
    case value of
	Num x => x
      | Jack => 10
      | Queen => 10
      | King => 10
      | Ace => 11

fun remove_card (cs, c, e) =
    let
	fun remove_card_sub (curCards, pastCards) =
	    case curCards of
		[] => raise e
	      | x :: x' => if x = c
			   then pastCards @ x'
			   else remove_card_sub (x', x :: pastCards)
    in
	remove_card_sub (cs, [])
    end

fun all_same_color lst =
    let
	val color = case lst of [] => Red 
			      | x :: x' => card_color x
	fun all_same_color_sub (restOfList, commonColor) =
	    case restOfList of
		[] => true
	      | x :: x' => if card_color (x) = commonColor
			   then all_same_color_sub (x', commonColor)
			   else false
    in
	all_same_color_sub (lst, color)
    end

fun sum_cards lst = 
    let
	fun sum_cards_sub (lst, total) = 
	    case lst of
		[] => total
 	      | x :: x' => sum_cards_sub (x', total + card_value x)
    in
	sum_cards_sub (lst, 0)
    end

fun score (lst, goal) = 
    let
	val value = sum_cards lst
	val pre_score = if value > goal 
			then 3 * (value - goal) 
			else (goal - value)
    in
	if all_same_color lst
	then pre_score div 2
	else pre_score
    end

fun officiate (cardList, moveList, goal) =
    let
	fun process_next_move (heldCards, nextMove, nextCards) = 
	    if sum_cards heldCards > goal
	    then score (heldCards, goal)
	    else
		case nextMove of
		    [] => score (heldCards, goal)
		  | x :: x' => case x of
				   Discard i => process_next_move (remove_card (heldCards, i, IllegalMove), x', nextCards)
				 | Draw => case nextCards of 
					       [] => score (heldCards, goal)
					     | y :: y' => process_next_move(y :: heldCards, x', y')
    in
	process_next_move ([], moveList, cardList)
    end
