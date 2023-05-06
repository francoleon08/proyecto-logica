:- module(proylcc, 
	[  
		join/4, /* Predicado para computar un camino */
		get_result_path/4, /* Predicado para obtener el resultado parcial de un camino */
		booster_colapser/3 /* Predicado para aplicar la funcion "Booster Colapser" */
	]).

:- use_module(library(random)).

/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, 
 * de combinar las celdas del camino Pathen la grilla Grid, con número de columnas NumOfColumns. 
 * El número 0 representa que la celda está vacía. 
 */ 
join(Grid, NumOfColumns, Path, RGrids):-	
	get_result_path(Grid, NumOfColumns, Path, R),
	set_path_grid(Grid, NumOfColumns, Path, R, RGridResult),
	set_zero_grid(Path, NumOfColumns, RGridResult, RGrid, RGridGravity),
	get_range(Grid, Low, High),
	generate_numbers_random(RGridGravity, Low, High, RGridFinish),
	RGrids = [Grid, RGridResult, RGrid, RGridGravity, RGridFinish].

/* ------------------OPERACIONES------------------ */

/* Se encarga de calcular la suma y el resultado del camino */
get_result_path(Grid, NumOfColumns, Path, Result):-
	calculate_sum_path(Grid, NumOfColumns, Path, R), /* Calcula la suma total del recorrido */
	pow_two(R, Result). /* Computa el resultado correcto */

/* Mediante un numero N computa la menor potencia de 2 mayor o igual a N */
pow_two(N, Pow) :-
	Pow is 2 ** floor(log(N) / log(2)).

/* Computa la suma de un Path dado */
calculate_sum_path(_, _, [], 0).
calculate_sum_path(Grid, NumOfColumns, [[X | Y] | Ps], R):-
	function_position_grid(X, Y, NumOfColumns, Pos),
	search_num_on_grid(Grid, Pos, N),
	calculate_sum_path(Grid, NumOfColumns, Ps, Aux),
	R is (Aux + N),
	!.

/*  
Mediante una grilla G y una posicion, obtiene el valor correspondiente.
*/
search_num_on_grid([G | _], 0, G).
search_num_on_grid([_ | Gs], Pos, N):-
	PosAux is (Pos-1),
	search_num_on_grid(Gs, PosAux, N),
	!.	

/* Genera un numero aleatorio 2^N, N perteneciente a [Lower , Upper). Lower > 0 */
/* 2^10 = 1024 */
generate_power_two(Lower, Upper, RandomNumber) :-	
	random_between(Lower, Upper, N), /* Genera un numero aleatorio entre [Lower, Upper] utilizando la libreria random */
	RandomNumber is 2 ** N. 
/* ---------------------------- */
/* Se encarga de buscar la ultima posicion del camino y setear el resultado */
set_path_grid(Grid, NumOfColumns, Path, Result, RGrid):-
	last(Path, [X | Y]), /* Busco la ultima posicion para setear el resultado */
	function_position_grid(X, Y, NumOfColumns, PosSet), /* Funcion para calcular la posicion en la grilla */
	set_result_path(PosSet, Grid, Result, RGrid). /* Setea el resultado del path en su posicion */

/* Mapea la posicion de un elemento en la grilla a su ubicacion en la Lista */
function_position_grid(X, Y, Columns, N):-
	N is (X*Columns + Y).

/* Intercambia un elemento por un elemento E en una posicion P de una Lista */
set_result_path(P, Grid, E, RGrid) :-
	swap_element_rec(P, Grid, E, RGrid).

swap_element_rec(0, [_ | Gs], E, [E | Gs]).
swap_element_rec(P, [G | Gs], E, [G | Rs]) :-
	P > 0,
	Next is P - 1,
	swap_element_rec(Next, Gs, E, Rs).

/* ---------------------------- */
/* Se encarga de poner en 0 los numeros correspondientes y reubicarlos*/
set_zero_grid(Path, NumOfColumns, RGrid, RGridResult, RGridGravity):-
	get_positions_path(Path, NumOfColumns, Posiciones), /* Obtengo las posiciones de cada path en la grilla */
	swap_positions(RGrid, Posiciones, RGridResult), /*intercambio cada posicion por un 0 en la grilla*/
	sort(Posiciones, PosicionesOrdenadas),
	set_gravity(RGridResult, NumOfColumns, PosicionesOrdenadas, RGridGravity).

/* Dado una lista de coordenadas, computa las posiciones correspondientes a cada una */
get_positions_path([_], _, []).
get_positions_path([[X, Y] | Ps], NumOfColumns, [L | Ls]):-
	function_position_grid(X, Y, NumOfColumns, L),
	get_positions_path(Ps, NumOfColumns, Ls).

/* Intercambia las posiciones elegidas por 0 */
swap_positions(Grid, Positions, RGrid) :-
	swap_positions_rec(Grid, Positions, RGrid, 0).
	
swap_positions_rec([], _, [], _).
swap_positions_rec([_ | Gs], Positions, [R | Rs], Index) :-
	member(Index, Positions),
	R is 0,
	NextIndex is Index + 1,
	swap_positions_rec(Gs, Positions, Rs, NextIndex).
swap_positions_rec([G | Gs], Positions, [G | Rs], Index) :-
	\+ member(Index, Positions),
	NextIndex is Index + 1,
	swap_positions_rec(Gs, Positions, Rs, NextIndex).

/* Dado una grilla con 0s, mueve todas sus apariciones a la parte superior */
set_gravity(Grid, _, [], Grid).
set_gravity(Grid, NumOfColumns, [P], RGrid):-
	swap_zero_for_top_rec(Grid, NumOfColumns, P, RGrid).
set_gravity(Grid, NumOfColumns, [P | Ps], RGrid):-
	swap_zero_for_top_rec(Grid, NumOfColumns, P, Grid_aux),
	set_gravity(Grid_aux, NumOfColumns, Ps, RGrid),
	!.

/* Setea una posicion PosPath en su lugar (efecto gravedad para los 0s) */
swap_zero_for_top_rec(Grid, NumOfColumns, PosPath, Grid):-
	PosPath < NumOfColumns.
swap_zero_for_top_rec(Grid, NumOfColumns, PosPath, RGrid):-
	swap_zero_for_top(Grid, NumOfColumns, PosPath, RGrid_aux),
	NewPosPath is (PosPath-NumOfColumns),
	swap_zero_for_top_rec(RGrid_aux, NumOfColumns, NewPosPath, RGrid).

/* Intercambia un elemento (ubicado en PosPath) por su elemento superior */
swap_zero_for_top(Grid, NumOfColumns, PosPath, RGrid):-	
	PosSwap is (PosPath-NumOfColumns),
	search_num_on_grid(Grid, PosPath, NumPath),
	search_num_on_grid(Grid, PosSwap, NumSwap),
	set_result_path(PosPath, Grid, NumSwap, Grid_aux),
	set_result_path(PosSwap, Grid_aux, NumPath, RGrid),
	!.

/* ---------------------------- */
/* 
	Dado una grilla, busca el maximo numero y retorna las potencias para generar numeros random 
	Low y High hacen referencia a un rango de potencias y no a la minima y maxima de la grilla.
*/
get_range(Grid, Low, High):-
	max_number_grid(Grid, Max),
	High is floor(log(Max)/log(2))-1,
	get_range_low(High, Low),
	!.

/* 
	Dado una potencia High retorna una potencia Low, formando un rango de potencias (N-8, N), siendo N>9.
	Si N<9 retorna el rango (1, N).
*/
get_range_low(High, Low):-
	High > 9,
	Low is (High-8).
get_range_low(_, 1).	

/* Dado una grilla retorna el maximo numero perteneciente a la misma */
max_number_grid([X], X).
max_number_grid([X|Xs], Max) :-
    max_number_grid(Xs, MaxResto),
    (X > MaxResto
    -> Max is X
    ;  Max is MaxResto
    ).

/* ---------------------------- */
/* 
	Genera una grilla con numeros pseudo-aleatorios.
	Los numeros generados son ubicados en las posiciones donde se encuentran los ceros.
*/
generate_numbers_random([], _, _, []).
generate_numbers_random([0 | Gs], Low, High, [R | Rs]):-
	generate_power_two(Low,High,R),
	generate_numbers_random(Gs, Low, High, Rs).
generate_numbers_random([G | Gs], Low, High, [G | Rs]):-
	generate_numbers_random(Gs, Low, High, Rs).

/* ------------------ FIN JOIN ------------------ */

/* ------------------ FUNCIONES BOOSTER COLAPSER ------------------ */
booster_colapser(Grid, NumOfColumns, RGrids):-
	get_grupos_elem_adyacents(Grid, NumOfColumns, Result),
	get_paths(Result, RGrids_All_Paths),
	concatenate_paths(RGrids_All_Paths, Pahts_concatenate),
	set_paths_zero_result(Grid, NumOfColumns, Pahts_concatenate, RGrids_zeros),
	last(RGrids_zeros, Grid_gravity_aux),
	get_positions_zeros(Grid_gravity_aux, 0, Positions_Zeros),
	set_gravity(Grid_gravity_aux, NumOfColumns, Positions_Zeros, Grid_Gravity),
	get_range(Grid, Low, High),
	generate_numbers_random(Grid_Gravity, Low, High, Grid_Finish),
	add_last(Grid_Gravity, RGrids_zeros, RGrids_aux),
	add_last(Grid_Finish, RGrids_aux, RGrids).

/* Dado una grilla, retorna las posiciones donde hay ceros */
get_positions_zeros([], _, []).
get_positions_zeros([0 | Gs], Index, [Index | Ps]):-
	NewIndex is (Index + 1),
	get_positions_zeros(Gs, NewIndex, Ps).
get_positions_zeros([_ | Gs], Index, Positions):-
	NewIndex is (Index + 1),
	get_positions_zeros(Gs, NewIndex, Positions).

/* 
	Setea el resultado de los camions en la grilla, eliminando los caminos mencionados
	Retorna una lista de Grillas, cada una correspondiente a la eliminacion de un camino
 */
set_paths_zero_result(_, _, [], []).
set_paths_zero_result(Grid, NumOfColumns, [P | Ps], [G | Gs]):-
	sort(P, P_ordered),
	nth0(0, P_ordered, Elem),
	nth0(Elem, Grid, N_aux),
	length(P_ordered, Size),
	N is (N_aux*Size),
	pow_two(N, Sum),
	last(P_ordered, Position_replace),
	replace(Grid, Position_replace, Sum, Grid_aux),
	set_zero_grid_positions(Grid_aux, P_ordered, G),
	set_paths_zero_result(G, NumOfColumns, Ps, Gs),
	!.
set_paths_zero_result(Grid, NumOfColumns, [_ | Ps], RGrids):-
	set_paths_zero_result(Grid, NumOfColumns, Ps, RGrids),
	!.

/* Dado una grilla, pone en 0 las posiciones recibidas */
set_zero_grid_positions(Grid, [_], Grid).
set_zero_grid_positions(Grid, [P | Ps], RGrid):-
	replace(Grid, P, 0, Grid_aux),
	set_zero_grid_positions(Grid_aux, Ps, RGrid).

/* Remplaza una elemento X en una lista en una posicion I */
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):-
    I > 0,
    NI is I-1,
    replace(T, NI, X, R).

/* Dado una lista de caminos, concatena aquellos que compartan elementos */
concatenate_paths([], []).
concatenate_paths([G | Gs], [R | Rs]):-
    concatenate(Gs, G, R, List_aux),
    concatenate_paths(List_aux, Rs).

/* Concatena dos listas en el caso de que compartan elementos */
concatenate([], List, List, []).
concatenate([G | Gs], List, R, List_aux):-
    comparten_elementos(G, List),
    unir_y_eliminar_repetidos([G, List], NewList),
    concatenate(Gs, NewList, R, List_aux),
    !.
concatenate([G | Gs], List, R, [G | Ts]):-
    concatenate(Gs, List, R, Ts),
    !.

/* Verifica si dos listas comparten elementos */
comparten_elementos(List1, List2):-
	member(E, List1),
	member(E, List2),
	!.

get_paths([], []).
get_paths([P | Ps], [R | Rs]):-
	unir_y_eliminar_repetidos(P, R),
	get_paths(Ps, Rs).

/* Dado una lista de listas, concatena aquellas que compartan elementos y elimina las apariciones repetidas de estos */
unir_y_eliminar_repetidos(ListaDeListas, Resultado) :-
	append(ListaDeListas, Lista),
	list_to_set(Lista, Resultado).

/* 
	Dado una grilla, retorna un Lista con la lista de caminos adyacentes,
	cada grupo de caminos correspondiente a un numero.
	Cada numero esta codificado como posiciones y no como el numero que representa.
*/
get_grupos_elem_adyacents(Grid, NumOfColumns, RGrids):-
	compute_of_groups(Grid, NumOfColumns, 0, RGrids_aux),
	refactor_grid_adyacents(RGrids_aux, RGrids),
	!.

/* Elimina las listas vacias dentro de una lista de listas */
refactor_grid_adyacents([], []).
refactor_grid_adyacents([[] | Gs], R):-
	refactor_grid_adyacents(Gs, R).
refactor_grid_adyacents([G | Gs], [G | Rs]):-
	refactor_grid_adyacents(Gs, Rs).

/* 
	Dado una lista de posiciones, genera los caminos correspondientes a cada numero de la grilla.
	Los caminos generados son la union del numero con sus posibles adyacentes.
	Todos los caminos generados son de tamaño 2.
 */
compute_of_groups([], _, _, []).
compute_of_groups([G | Gs], NumOfColumns, Index, [GridGroup | RGrids]):-
	get_positions_grid_elem([G | Gs], G, Index, Positions), /* Posiciones del elemento G en la grilla */
	NewIndex is (Index + 1),
	Positions = [P | _],
	get_groups(P, Positions, NumOfColumns, GridGroup), /* Dado las posiciones anteriores, formo los caminos adyacentes */
	compute_of_groups(Gs, NumOfColumns, NewIndex, RGrids),
	!.
compute_of_groups([_ | Gs], NumOfColumns, Index, RGrids):- /* Caso donde el elemento no tiene adyacentes */
	NewIndex is (Index + 1),
	compute_of_groups(Gs, NumOfColumns, NewIndex, RGrids),
	!.

/* 
	Genera una lista de caminos posibles de tamaño 2 para un numero ubicado en la posicion Pos.
	Es posible trabajar directamente con las posiciones para computar si son adyacentes, ya que 
	la lista de posiciones recibida corresponden a un mismo numero.
*/
get_groups(_, [], _, []).
get_groups(Pos, [P | Ps], NumOfColumns, [[Pos,P] | Ls]):-
	is_adyacent(Pos, P, NumOfColumns), /* Si P es una posicion adyacente a Pos */
	get_groups(Pos, Ps, NumOfColumns, Ls).
get_groups(Pos, [_ | Ps], NumOfColumns, ListGroupAdyacents):-
	get_groups(Pos, Ps, NumOfColumns, ListGroupAdyacents).


/* Retorna las posiciones de un elemento Elem en la grilla */
get_positions_grid_elem([], _, _, []).
get_positions_grid_elem([Elem | Gs], Elem, Index, [Index | Ps]):-
	NewIndex is (Index + 1),
	get_positions_grid_elem(Gs, Elem, NewIndex, Ps).
get_positions_grid_elem([_ | Gs], Elem, Index, PositionsElem):-
	NewIndex is (Index + 1),
	get_positions_grid_elem(Gs, Elem, NewIndex, PositionsElem).


/* POSICIONES DE LA GRILLA */
/* Si la posicion P1 se ubica a la izquierda */
is_adyacent(P1, P2, NumOfColumns):-
	P1 mod NumOfColumns =:= 0,
	(P2 + 1) mod NumOfColumns =\= 0,
	case_1(P1, P2, NumOfColumns),
	!.

/* Si la posicion P1 se ubica a la derecha */
is_adyacent(P1, P2, NumOfColumns):-
	(P1 + 1) mod NumOfColumns =:= 0,
	P2 mod NumOfColumns =\= 0,
	(P2 - 1) =\= P1,
	case_2(P1, P2, NumOfColumns),
	!.

/* Si la posicion P1 se ubica al centro */
is_adyacent(P1, P2, NumOfColumns):-	
	P1 mod NumOfColumns =\= 0,
	(P1 + 1) mod NumOfColumns =\= 0,
	case_3(P1, P2, NumOfColumns),
	!.

/* CASOS PARA CADA POSCION DE LA GRILLA */
/* Si la posicion P1 se ubica a la izquierda */
case_1(P1, P2, NumOfColumns):-
	P2 =:= (P1 + 1);
	P2 =:= (P1 + NumOfColumns);
	P2 =:= (P1 + NumOfColumns + 1).

/* Si la posicion P1 se ubica en la derecha */
case_2(P1, P2, NumOfColumns):-
	P2 =:= (P1 + NumOfColumns - 1);
	P2 =:= (P1 + NumOfColumns).

/* Si la posicion P1 esta en el centro */
case_3(P1, P2, NumOfColumns):-
	P2 =:= (P1 + 1);
	P2 =:= (P1 + NumOfColumns - 1);
	P2 =:= (P1 + NumOfColumns);
	P2 =:= (P1 + NumOfColumns + 1).	

/* ------------------ FIN BOOSTER COLAPSER ------------------ */

/* -------------- OPERACIONES SOBRE LISTAS -------------- */
/* Dado una lista L, agrega el elemento E al final de L */
add_last(E ,L, R):-
	concat(L, [E], R).

/* Concatena dos listas */		
concat([], Xs, Xs).
concat([X | Xs], Ys, [X | Zs]):-
	concat(Xs, Ys, Zs).

/* Intercambia dos elementos en una lista */
swap(List, Pos1, Pos2, Result) :-
	nth0(Pos1, List, Elem1, Temp1),
	nth0(Pos2, Temp1, Elem2, Temp2),
	nth0(Pos1, Temp2, Elem2, Temp3),
	nth0(Pos2, Temp3, Elem1, Result).