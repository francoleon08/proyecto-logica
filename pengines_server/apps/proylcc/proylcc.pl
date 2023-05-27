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
	RGrids = [RGridResult, RGrid, RGridGravity, RGridFinish].

/* ------------------OPERACIONES------------------ */

/* Se encarga de calcular la suma y el resultado del camino */
get_result_path(Grid, NumOfColumns, Path, Result):-
	calculate_sum_path(Grid, NumOfColumns, Path, R), /* Calcula la suma total del recorrido */
	pow_two(R, Result). /* Computa el resultado correcto */

/* Mediante un numero N computa la menor potencia de 2 mayor o igual a N */
pow_two(N, Pow) :-
	Pow is 2 ** ceiling(log(N) / log(2)).

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
get_range(Grid, Low, NewHigh):-
	max_number_grid(Grid, Max),
	High is floor(log(Max)/log(2))-1,
	get_range_low(High, NewHigh, Low),
	!.

/* 
	Dado una potencia High retorna una potencia Low, formando un rango de potencias (N-8, N), siendo N>9.
	Si N<9 retorna el rango (1, N).
*/
get_range_low(High, 5, 2):-
	High < 9.
get_range_low(High, NewHigh, Low):-
	High > 15,
	NewHigh is (floor(High/2)),
	Low is (ceiling(NewHigh/3)).

get_range_low(High, NewHigh, Low):-
	NewHigh is (floor(High/2)),
	Low is (floor(NewHigh/4)).

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
	concatenate_paths(RGrids_All_Paths, Pahts_concatenate_aux),
	concatenate_paths(Pahts_concatenate_aux, Pahts_concatenate), /* Segundo llamado por si ocurrio un caso donde la transitividad entre caminos no logro darse */
	set_paths_zero_result(Grid, NumOfColumns, Pahts_concatenate, RGrids_zeros),
	last(RGrids_zeros, Grid_gravity_aux),
	get_positions_zeros(Grid_gravity_aux, 0, Positions_Zeros),
	set_gravity(Grid_gravity_aux, NumOfColumns, Positions_Zeros, Grid_Gravity),
	get_range(Grid, Low, High),
	generate_numbers_random(Grid_Gravity, Low, High, Grid_Finish),
	append(RGrids_zeros, [Grid_Gravity], RGrids_aux),
	append(RGrids_aux, [Grid_Finish], RGrids).

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
	get_positions_grid_elem(Gs, Elem, NewIndex, Ps),
	!.
get_positions_grid_elem([_ | Gs], Elem, Index, PositionsElem):-
	NewIndex is (Index + 1),
	get_positions_grid_elem(Gs, Elem, NewIndex, PositionsElem).


/* POSICIONES DE LA GRILLA */
/* Si la posicion P1 se ubica a la izquierda */
is_adyacent(P1, P2, NumOfColumns):-
	P1 mod NumOfColumns =:= 0,
	%(P2 + 1) mod NumOfColumns =\= 0,
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
	P2 =:= (P1 - NumOfColumns);
	P2 =:= (P1 - NumOfColumns + 1);
	P2 =:= (P1 + 1);
	P2 =:= (P1 + NumOfColumns);
	P2 =:= (P1 + NumOfColumns + 1).

/* Si la posicion P1 se ubica en la derecha */
case_2(P1, P2, NumOfColumns):-
	P2 =:= (P1 - 1);
	P2 =:= (P1 - NumOfColumns);
	P2 =:= (P1 - NumOfColumns - 1);
	P2 =:= (P1 + NumOfColumns - 1);
	P2 =:= (P1 + NumOfColumns).

/* Si la posicion P1 esta en el centro */
case_3(P1, P2, NumOfColumns):-
	P2 =:= (P1 - NumOfColumns - 1);
	P2 =:= (P1 - NumOfColumns);
	P2 =:= (P1 - NumOfColumns + 1);
	P2 =:= (P1 - 1);
	P2 =:= (P1 + 1);
	P2 =:= (P1 + NumOfColumns - 1);
	P2 =:= (P1 + NumOfColumns);
	P2 =:= (P1 + NumOfColumns + 1).	

/* ------------------ FIN BOOSTER COLAPSER ------------------ */

/* Intercambia dos elementos en una lista */
swap(List, Pos1, Pos2, Result) :-
	nth0(Pos1, List, Elem1, Temp1),
	nth0(Pos2, Temp1, Elem2, Temp2),
	nth0(Pos1, Temp2, Elem2, Temp3),
	nth0(Pos2, Temp3, Elem1, Result).

/* ------------------ ETAPA 2 -------------------- */
/**
 * Dado una grilla con su numero de columnas, busca y retorna el mejor camino y el resultado de su sumatoria.
*/
best_move(Grid, NumOfColumns, MaxPath, ResultMaxPath):-	
	find_paths(Grid, Grid, NumOfColumns, 0, ListPaths),
	flatten_list(ListPaths, ListPathsFlatten),
	length(ListPathsFlatten, Length),
	Length > 0,
	max_path(Grid, NumOfColumns, ListPathsFlatten, 0, MaxPaths),
	last(MaxPaths, MaxPath),
	get_result_path(Grid, NumOfColumns, MaxPath, ResultMaxPath).


/**  
 * Construye todos los caminos existentes dentro de la grilla.
 * Se construyen por fuerza bruta.
*/
find_paths(_, [], _, _, []).
find_paths(Grid, [G | Gs], NumOfColumns, Index, [PathFinish | Next]):-
	positions_adyacentes(Index, NumOfColumns, PositionsAdyacents),	
	get_coordinate(Index, NumOfColumns, Cordinate),
	create_path(Grid, NumOfColumns, G, PositionsAdyacents, [Index], [Cordinate], PathFinish),		
	length(PathFinish, Length),
	Length > 1,
	NewIndex is (Index + 1),
	find_paths(Grid, Gs, NumOfColumns, NewIndex, Next),
	!.
find_paths(Grid, [_| Gs], NumOfColumns, Index, Paths):-
	NewIndex is (Index + 1),
	find_paths(Grid, Gs, NumOfColumns, NewIndex, Paths).

/**  
 * Dado una posicion P y el numero de columnas de la grilla, computa su posiciones [X,Y].
*/
get_coordinate(P, NumOfColumns, [X, Y]):-
	X is (floor(P / NumOfColumns)),
	Y is (P mod NumOfColumns).

/**  
 * Transforma una lista de listas de listas en una lista de listas.
 * [ [[1,2][3,4]], [[5,6]] ] ==> [ [1,2], [3,4], [5,6] ].
*/
flatten_list([], []).
flatten_list([P | Ps], FlatResult) :-
    flatten_list(Ps, FlatP),
    append(P, FlatP, FlatResult).

/**  
 * Dado una grilla y un elemento, busca todos los caminos que puedan formarse.
 * Realiza la busqueda en profundidad (DFS).
 * E = elemento desde donde parte la busqueda.
 * [A | As] = lista de elementos adyacentes a E.
 * Visited = lista de bloques visitados por el camino.
 * Path = camino construido hasta el momento.
 * PathsResult = lista de caminos obtenidos partiendo del elemento E.
*/
create_path(_, _, _, [], _, Path, [Path]).
create_path(Grid, NumOfColumns, E, [A | As], Visited, Path, PathsResult):-
	length(Path, SizePath),
	SizePath =:= 1,
	not(member(A,Visited)), /* Si no visite la posicion */	
	nth0(A, Grid, Elem), /* Elemento en la posicion A de la grilla */
	Elem =:= E,
	create_paths_aux(Grid, NumOfColumns, E, Elem, [A | As], Visited, Path, PathsResult).
create_path(Grid, NumOfColumns, E, [A | As], Visited, Path, PathsResult):-
	length(Path, SizePath),
	SizePath > 1,
	not(member(A,Visited)), /* Si no visite la posicion */	
	nth0(A, Grid, Elem), /* Elemento en la posicion A de la grilla */
	(Elem =:= E ; Elem =:= E*2),
	create_paths_aux(Grid, NumOfColumns, E, Elem, [A | As], Visited, Path, PathsResult).
create_path(Grid, NumOfColumns, E, [_ | As], Visited, Path, ListPaths):-
	create_path(Grid, NumOfColumns, E, As, Visited, Path, ListPaths).

/**  
 * Predicado para realizar la busqueda de caminos. Recorre las posiciones adyacentes.
*/
create_path_adyacent(Grid, NumOfColumns, E, [A | As], Visited, Path, ListPaths):-
	create_path(Grid, NumOfColumns, E, [A | As], Visited, Path, ListPaths).

/**  
 * Predicado modular para los casos de create_path
*/
create_paths_aux(Grid, NumOfColumns, E, Elem, [A | As], Visited, Path, PRFinish):-
	append([A], Visited, Vs), /* Marco como visitada */
	get_coordinate(A, NumOfColumns, Cordinate),
	append(Path, [Cordinate], PathResult), /* Agrego la posicion al camino */
	positions_adyacentes(A, NumOfColumns, PositionsAdyacents),
	create_path_adyacent(Grid, NumOfColumns, Elem, PositionsAdyacents, Vs, PathResult, PR1),
	create_path(Grid, NumOfColumns, E, As, Visited, Path,  PR2),	
	append([PathResult], PR1, PRaux),
	append(PRaux, PR2, PRFinish),
	!.

/**  
 * Dado una posicion, retorna una lista de posiciones adyacentes a la misma.
*/
positions_adyacentes(Pos, NumOfColumns, Positions):-
	P1 is Pos + NumOfColumns - 1,
	P2 is Pos + NumOfColumns,
	P3 is Pos + NumOfColumns + 1,
	P4 is Pos + 1,
	P5 is Pos - NumOfColumns + 1,
	P6 is Pos - NumOfColumns,
	P7 is Pos - NumOfColumns - 1,
	P8 is Pos - 1,
	check_positions([P1, P2, P3, P4, P5, P6, P7, P8], Pos, NumOfColumns, Positions).

/**  
 * Dado una lista de posiciones, elimina aquellas que no sean adyacentes a Pos o aquellas que no sean > 0.
*/	
check_positions([], _, _, []).
check_positions([X | Xs], Pos, NumOfColumns, [X | Ps]):-
	X >= 0,
	is_adyacent(Pos, X, NumOfColumns),
	check_positions(Xs, Pos, NumOfColumns, Ps),
	!.
check_positions([_ | Xs], Pos, NumOfColumns, Ps):-
	check_positions(Xs, Pos, NumOfColumns, Ps).

/**  
 * Dado una lista de caminos, crea una lista con los caminos mayores a un resultado MaxResult.
 * El ultimo elemento de la lista es el camino con el mayor resultado.
 * En el caso de que varios caminos computen el mismo resultado, elije el primero que encuentra.
 * Se realza de esta forma, porque el tiempo de ejecucion disminuye considerablemente.
*/
max_path(_, _, [], _, []).
max_path(Grid, NumOfColumns, [MaxPath | Ps], MaxResult, [MaxPath | NewPath]):-
	get_result_path(Grid, NumOfColumns, MaxPath, Result),
	Result > MaxResult,
	max_path(Grid, NumOfColumns, Ps, Result, NewPath),
	!.
max_path(Grid, NumOfColumns, [_ | Ps], MaxResult, MaxPath):-
	max_path(Grid, NumOfColumns, Ps, MaxResult, MaxPath).

best_move_adyacent(Grid, NumOfColumns, Path, Result):-
	get_grid_falling(Grid, GridFalling),
	find_paths(Grid, Grid, NumOfColumns, 0, AllPaths),
	flatten_list(AllPaths, AllPathsFlatten),
	find_path(Grid, NumOfColumns, GridFalling, AllPathsFlatten, Path, Result).


/**  
 * Dado una grilla Grid, retorna un grilla ordenada de forma descendente.
*/
get_grid_falling(Grid, GridResult):-
	refactor_grid(Grid, 0, GridPositions),
	sort(GridPositions, GridAux),
	reverse(GridAux, GridResult).

/**  
 * Transforma una lista en una lista donde cada componente de la grilla es [E, p], 
 * donde p es la posicion en la lista y E el elemento.
*/
refactor_grid([], _, []).
refactor_grid([G | Gs], Index, [[G, Index] | Rs]):-
	NewIndex is Index+1,
	refactor_grid(Gs, NewIndex, Rs).	

/**  
 * Busca un camino adyacente a alguna de las posiciones maximas de la grilla.
*/
find_path(_, _, [], _, [], 0).
find_path(Grid, NumOfColumns, [[E, P] | _], Paths, Path, E):-
	find_paths_equals_result(Paths, Grid, NumOfColumns, E, PathsAdyacents),
	find_path_adyacent(P, NumOfColumns, PathsAdyacents, Path),
	length(Path, Length),
	Length > 1,
	!.
find_path(Grid, NumOfColumns, [_ | GRs], Paths, Path, E):-
	find_path(Grid, NumOfColumns, GRs, Paths, Path, E).

/**  
 * Dado una lista de caminos, retorna aquellos caminos que su resultado coincida
 * con el recibido por parametro.
*/
find_paths_equals_result([], _, _, _, []).
find_paths_equals_result([P | Ps], Grid, NumOfColumns, Result, [P | Rs]):-
	length(P, Length),
	Length > 1,
	get_result_path(Grid, NumOfColumns, P, Result),
	find_paths_equals_result(Ps, Grid, NumOfColumns, Result, Rs).
find_paths_equals_result([_ | Ps], Grid, NumOfColumns, Result, Paths):-
	find_paths_equals_result(Ps, Grid, NumOfColumns, Result, Paths).

/**  
 * Recorre la lista de caminos en busca de uno en la que su ultima posicion
 * sea adyacente a la posicion del elemento en la grilla.
*/
find_path_adyacent(_, _, [], []).
find_path_adyacent(Pos, NumOfColumns, [P | _], P):-	
	equals_adyacent_position(Pos, NumOfColumns, P).
find_path_adyacent(Pos, NumOfColumns, [_ | Ps], P):-
	find_path_adyacent(Pos, NumOfColumns, Ps, P).

/**  
 * Dado una posicion y un camino, verifica si la posicion final del camino es adyacente a la posicion recibida,
 * y que la posicion no pertenezca al camino.
*/
equals_adyacent_position(Pos, NumOfColumns, Path):-
	last(Path, [X | Y]),
	function_position_grid(X, Y, NumOfColumns, LastPosition),
	is_adyacent(Pos, LastPosition, NumOfColumns),
	get_coordinate(Pos, NumOfColumns, PosCoordinate),
	not(member(PosCoordinate, Path)).