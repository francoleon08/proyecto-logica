:- module(proylcc, 
	[  
		join/4
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
	%get_rango(Grid, Low, High),
	generate_numbers_random(RGridGravity, 1, 10, RGridFinish),
	RGrids = [Grid, RGridResult, RGrid, RGridGravity, RGridFinish].

/* ------------------OPERACIONES------------------ */

/* Se encarga de calcular la suma y el resultado del camino */
get_result_path(Grid, NumOfColumns, Path, Result):-
	calculate_sum_path(Grid, NumOfColumns, Path, R), /* Calcula la suma total del recorrido */
	pow_two(R, Result). /* Computa el resultado correcto */

/* Se encarga de buscar la ultima posicion del camino y setear el resultado */
set_path_grid(Grid, NumOfColumns, Path, Result, RGrid):-
	last_position(Path, [X | Y]), /* Busco la ultima posicion para setear el resultado */
	function_position_grid(X, Y, NumOfColumns, PosSet), /* Funcion para calcular la posicion en la grilla */
	set_result_path(PosSet, Grid, Result, RGrid). /* Setea el resultado del path en su posicion */

/* Se encarga de poner en 0 los numeros correspondientes y reubicarlos*/
set_zero_grid(Path, NumOfColumns, RGrid, RGridResult, RGridGravity):-
	get_positions(Path, NumOfColumns, Posiciones), /* Obtengo las posiciones de cada path en la grilla */
	intercambiar_posiciones(RGrid, Posiciones, RGridResult), /*intercambio cada posicion por un 0 en la grilla*/
	quicksort(Posiciones, PosicionesOrdenadas),
	set_gravity(RGridResult, NumOfColumns, PosicionesOrdenadas, RGridGravity).

/* Dado una grilla con 0s, mueve todas sus apariciones a la parte superior */
set_gravity(Grid, NumOfColumns, [], Grid).
set_gravity(Grid, NumOfColumns, [P], RGrid):-
	swap_zero_rec(Grid, NumOfColumns, P, RGrid).
set_gravity(Grid, NumOfColumns, [P | Ps], RGrid):-
	swap_zero_rec(Grid, NumOfColumns, P, Grid_aux),
	set_gravity(Grid_aux, NumOfColumns, Ps, RGrid),
	!.

/* Setea una posicion PosPath en su lugar (efecto gravedad para los 0s) */
swap_zero_rec(Grid, NumOfColumns, PosPath, Grid):-
	PosPath < NumOfColumns.
swap_zero_rec(Grid, NumOfColumns, PosPath, RGrid):-
	swap_zero(Grid, NumOfColumns, PosPath, RGrid_aux),
	NewPosPath is (PosPath-NumOfColumns),
	swap_zero_rec(RGrid_aux, NumOfColumns, NewPosPath, RGrid).

/* Intercambia un elemento (ubicado en PosPath) por su elemento superior */
swap_zero(Grid, NumOfColumns, PosPath, RGrid):-	
	PosSwap is (PosPath-NumOfColumns),
	search_num_on_grid(Grid, PosPath, NumPath),
	search_num_on_grid(Grid, PosSwap, NumSwap),
	set_result_path(PosPath, Grid, NumSwap, Grid_aux),
	set_result_path(PosSwap, Grid_aux, NumPath, RGrid),
	!.

get_rango(Grid, Low, High):-
	max_number(Grid, Max),
	High is round(sqrt(Max)),
	get_rango_low(High, Low),
	!.

get_rango_low(High, 1).	
get_rango_low(High, Low):-
	High > 7,
	Low is (High-6).

max_number([X], X).
max_number([X|Xs], Max) :-
    max_number(Xs, MaxResto),
    (X > MaxResto
    -> Max is X
    ;  Max is MaxResto
    ).

/* Genera una grilla con numeros random */
generate_numbers_random([], _, _, []).
generate_numbers_random([0 | Gs], Low, High, [R | Rs]):-
	generate_power_two(Low,High,R),
	generate_numbers_random(Gs, Low, High, Rs).
generate_numbers_random([G | Gs], Low, High, [G | Rs]):-
	generate_numbers_random(Gs, Low, High, Rs).

/* ------------------FUNCIONES------------------ */
/* Dado una lista de coordenadas, computa las posiciones correspondientes a cada una */
get_positions([Ps], _, []).
get_positions([[X, Y] | Ps], NumOfColumns, [L | Ls]):-
	function_position_grid(X, Y, NumOfColumns, L),
	get_positions(Ps, NumOfColumns, Ls).

/* Dado una lista, retorna el ultimo elemento */
last_position([Ps], Ps).
last_position([P | Ps], Pos):-
	last_position(Ps, Pos),
	!.

/* Computa la suma de un Path dado */
calculate_sum_path(Grid, NumOfColumns, [], 0).
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

/* Genera un numero aleatorio 2^N, N perteneciente a (Lower , Upper). Lower > 0 */
/* 2^10 = 1024 */
generate_power_two(Lower, Upper, RandomNumber) :-
	random(Lower, Upper, N),
	RandomNumber is 2 ** N. 

/* ------------------FUNCIONES A REFACOTRIZAR------------------ */
/* Intercambia un elemento por NuevoElemento en una posicion P de una Lista  ====> ¡Refactorizar! */
set_result_path(P, Lista, NuevoElemento, NuevaLista) :-
	intercambiar_elemento_rec(P, Lista, NuevoElemento, NuevaLista, []).
	
intercambiar_elemento_rec(0, [_ | Resto], NuevoElemento, [NuevoElemento | Resto], _).
intercambiar_elemento_rec(P, [Cabeza | Resto], NuevoElemento, [Cabeza | NuevaResto], Acumulador) :-
	P > 0,
	Siguiente is P - 1,
	intercambiar_elemento_rec(Siguiente, Resto, NuevoElemento, NuevaResto, [Cabeza | Acumulador]).
	
/* Intercambia las posiciones elegidas por 0 ===> ¡Refactorizar! */
intercambiar_posiciones(Lista, Posiciones, NuevaLista) :-
	intercambiar_posiciones_aux(Lista, Posiciones, NuevaLista, 0).
	
intercambiar_posiciones_aux([], _, [], _).
intercambiar_posiciones_aux([Cabeza | Resto], Posiciones, [NuevoCabeza | NuevaResto], Indice) :-
	member(Indice, Posiciones),
	NuevoCabeza is 0,
	SiguienteIndice is Indice + 1,
	intercambiar_posiciones_aux(Resto, Posiciones, NuevaResto, SiguienteIndice).
intercambiar_posiciones_aux([Cabeza | Resto], Posiciones, [Cabeza | NuevaResto], Indice) :-
	\+ member(Indice, Posiciones),
	SiguienteIndice is Indice + 1,
	intercambiar_posiciones_aux(Resto, Posiciones, NuevaResto, SiguienteIndice).

/* Intercambia dos elementos en una lista */
swap(List, Pos1, Pos2, Result) :-
	nth0(Pos1, List, Elem1, Temp1),
	nth0(Pos2, Temp1, Elem2, Temp2),
	nth0(Pos1, Temp2, Elem2, Temp3),
	nth0(Pos2, Temp3, Elem1, Result).

% Caso base: una lista vacía está ordenada
quicksort([], []).

% Caso recursivo: ordenar la lista
quicksort([Pivot | Resto], Sorted) :-
    partition(Pivot, Resto, Menores, Mayores),
    quicksort(Menores, MenoresOrdenados),
    quicksort(Mayores, MayoresOrdenados),
    append(MenoresOrdenados, [Pivot | MayoresOrdenados], Sorted).

% Predicado auxiliar para particionar la lista en menores y mayores
partition(_, [], [], []).

partition(Pivot, [X | Resto], [X | Menores], Mayores) :-
    X =< Pivot,
    partition(Pivot, Resto, Menores, Mayores).

partition(Pivot, [X | Resto], Menores, [X | Mayores]) :-
    X > Pivot,
    partition(Pivot, Resto, Menores, Mayores).