:- module(proylcc, 
	[  
		join/4,
		get_result_path/4,
		set_path_grid/5,
		set_gravity/4,
		swap_zero_for_top_rec/4,
		swap_zero_for_top/4,
		get_range/3,
		get_range_low/2,
		max_number_grid/2,
		generate_numbers_random/4,
		get_positions_path/3,
		last_position_path/2,
		calculate_sum_path/4,
		pow_two/2,
		search_num_on_grid/3,
		function_position_grid/4,
		generate_power_two/3,
		set_result_path/4,
		swap_element_rec/5,
		swap_positions/3,
		swap_positions_rec/4,
		swap/4,
		quicksort/2,
		partition/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 
join(Grid, NumOfColumns, Path, RGrids):-	
	get_result_path(Grid, NumOfColumns, Path, R),
	set_path_grid(Grid, NumOfColumns, Path, R, RGridResult),
	set_zero_grid(Path, NumOfColumns, RGridResult, RGrid, RGridGravity),
	%get_range(Grid, Low, High),
	generate_numbers_random(RGridGravity, 1, 10, RGridFinish),
	RGrids = [Grid, RGridResult, RGrid].

/* ------------------OPERACIONES------------------ */

/* Se encarga de calcular la suma y el resultado del camino */
get_result_path(Grid, NumOfColumns, Path, Result):-
	calculate_sum_path(Grid, NumOfColumns, Path, R), /* Calcula la suma total del recorrido */
	pow_two(R, Result). /* Computa el resultado correcto */

/* Se encarga de buscar la ultima posicion del camino y setear el resultado */
set_path_grid(Grid, NumOfColumns, Path, Result, RGrid):-
	last_position_path(Path, [X | Y]), /* Busco la ultima posicion para setear el resultado */
	function_position_grid(X, Y, NumOfColumns, PosSet), /* Funcion para calcular la posicion en la grilla */
	set_result_path(PosSet, Grid, Result, RGrid). /* Setea el resultado del path en su posicion */

/* Se encarga de poner en 0 los numeros correspondientes y reubicarlos*/
set_zero_grid(Path, NumOfColumns, RGrid, RGridResult, RGridGravity):-
	get_positions_path(Path, NumOfColumns, Posiciones), /* Obtengo las posiciones de cada path en la grilla */
	swap_positions(RGrid, Posiciones, RGridResult), /*intercambio cada posicion por un 0 en la grilla*/
	quicksort(Posiciones, PosicionesOrdenadas),
	set_gravity(RGridResult, NumOfColumns, PosicionesOrdenadas, RGridGravity).

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

get_range(Grid, Low, High):-
	max_number_grid(Grid, Max),
	High is round(sqrt(Max)),
	get_range_low(High, Low),
	!.

get_range_low(_, 1).	
get_range_low(High, Low):-
	High > 7,
	Low is (High-6).

max_number_grid([X], X).
max_number_grid([X|Xs], Max) :-
    max_number_grid(Xs, MaxResto),
    (X > MaxResto
    -> Max is X
    ;  Max is MaxResto
    ).

/* Genera una grilla con numeros pseudo aleatorios */
generate_numbers_random([], _, _, []).
generate_numbers_random([0 | Gs], Low, High, [R | Rs]):-
	generate_power_two(Low,High,R),
	generate_numbers_random(Gs, Low, High, Rs).
generate_numbers_random([G | Gs], Low, High, [G | Rs]):-
	generate_numbers_random(Gs, Low, High, Rs).

/* ------------------FUNCIONES------------------ */
/* Dado una lista de coordenadas, computa las posiciones correspondientes a cada una */
get_positions_path([_], _, []).
get_positions_path([[X, Y] | Ps], NumOfColumns, [L | Ls]):-
	function_position_grid(X, Y, NumOfColumns, L),
	get_positions_path(Ps, NumOfColumns, Ls).

/* Dado una lista, retorna el ultimo elemento */
last_position_path([Ps], Ps).
last_position_path([_ | Ps], Pos):-
	last_position_path(Ps, Pos),
	!.

/* Computa la suma de un Path dado */
calculate_sum_path(_, _, [], 0).
calculate_sum_path(Grid, NumOfColumns, [[X | Y] | Ps], R):-
	function_position_grid(X, Y, NumOfColumns, Pos),
	search_num_on_grid(Grid, Pos, N),
	calculate_sum_path(Grid, NumOfColumns, Ps, Aux),
	R is (Aux + N),
	!.

/* Mediante un numero N computa la menor potencia de 2 mayor o igual a N */
pow_two(N, Pow) :-
	Pow is 2 ** ceiling(log(N) / log(2)).

/*  
Mediante una grilla G y una posicion, obtiene el valor correspondiente.
*/
search_num_on_grid([G | _], 0, G).
search_num_on_grid([_ | Gs], Pos, N):-
	PosAux is (Pos-1),
	search_num_on_grid(Gs, PosAux, N),
	!.	

/* Mapea la posicion de un elemento en la grilla a su ubicacion en la Lista */
function_position_grid(X, Y, Columns, N):-
	N is (X*Columns + Y).

/* Genera un numero aleatorio 2^N, N perteneciente a [Lower , Upper). Lower > 0 */
/* 2^10 = 1024 */
generate_power_two(Lower, Upper, RandomNumber) :-
	random(Lower, Upper, N),
	RandomNumber is 2 ** N. 

/* ------------------FUNCIONES A REFACOTRIZAR------------------ */
/* Intercambia un elemento por un elemento E en una posicion P de una Lista  ====> ¡Refactorizar! */
set_result_path(P, Grid, E, RGrid) :-
	swap_element_rec(P, Grid, E, RGrid, []).
	
swap_element_rec(0, [_ | Gs], E, [E | Gs], _).
swap_element_rec(P, [G | Gs], E, [G | Rs], S) :-
	P > 0,
	Next is P - 1,
	swap_element_rec(Next, Gs, E,Rs, [G | S]).
	
/* Intercambia las posiciones elegidas por 0 ===> ¡Refactorizar! */
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

/* Intercambia dos elementos en una lista */
swap(List, Pos1, Pos2, Result) :-
	nth0(Pos1, List, Elem1, Temp1),
	nth0(Pos2, Temp1, Elem2, Temp2),
	nth0(Pos1, Temp2, Elem2, Temp3),
	nth0(Pos2, Temp3, Elem1, Result).

% Caso base: una lista vacía está ordenada
quicksort([], []).

% Caso recursivo: ordenar la lista
quicksort([Pivot | Gs], Sorted) :-
    partition(Pivot, Gs, Menores, Mayores),
    quicksort(Menores, MenoresOrdenados),
    quicksort(Mayores, MayoresOrdenados),
    append(MenoresOrdenados, [Pivot | MayoresOrdenados], Sorted).

% Predicado auxiliar para particionar la lista en menores y mayores
partition(_, [], [], []).

partition(Pivot, [X | Gs], [X | Menores], Mayores) :-
    X =< Pivot,
    partition(Pivot, Gs, Menores, Mayores).

partition(Pivot, [X | Gs], Menores, [X | Mayores]) :-
    X > Pivot,
    partition(Pivot, Gs, Menores, Mayores).