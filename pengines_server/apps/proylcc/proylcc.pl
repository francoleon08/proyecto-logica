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
	set_path_grid(Grid, NumOfColumns, Path, R, RGridAux),
	set_zero_grid(Path, NumOfColumns, RGridAux, RGrid),
	RGrids = [Grid, RGrid].

/* ------------------OPERACIONES------------------ */

/* Se encarga de calcular la suma y el resultado del camino */
get_result_path(Grid, NumOfColumns, Path, Result):-
	calculate_sum_path(Grid, NumOfColumns, Path, R), /* Calcula la suma total del recorrido */
	pow_two(R, Result). /* Computa el resultado correcto */

/* Se encarga de buscar la ultima posicion del camino y setear el resultado */
set_path_grid(Grid, NumOfColumns, Path, Result, RGrid):-
	last_position(Path, [X | Y]), /* Busco la ultima posicion para setear el resultado */
	function_position_grid(X, Y, NumOfColumns, PosSwap), /* Funcion para calcular la posicion en la grilla */
	set_result_path(PosSwap, Grid, Result, RGrid). /* Setea el resultado del path en su posicion */

/* Se encarga de poner en 0 los numeros correspondientes */
set_zero_grid(Path, NumOfColumns, RGrid, RGridResult):-
	get_positions(Path, NumOfColumns, Posiciones), /* Obtengo las posiciones de cada path en la grilla */
	intercambiar_posiciones(RGrid, Posiciones, RGridResult). /* intercambio cada posicion por un 0 en la grilla */


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
/* Intercambia un elemento por NuecoElemento en una posicion P de una Lista  ====> ¡Refactorizar! */
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



/* ------------------CODIGO COMENTADO------------------ */
%JOIN
%calculate_sum_path(Grid, NumOfColumns, Path, R), /* Calcula la suma total del recorrido */
	%pow_two(R, Result), /* Computa el resultado correcto */

	%last_position(Path, [X | Y]), /* Busco la ultima posicion para setear el resultado */
	%function_position_grid(X, Y, NumOfColumns, PosSwap), /* Funcion para calcular la posicion en la grilla */
	%set_result_path(PosSwap, Grid, Result, RGridAux), /* Setea el resultado del path en su posicion */

	%get_positions(Path, NumOfColumns, Posiciones), /* Obtengo las posiciones de cada path en la grilla */
	%intercambiar_posiciones(RGridAux, Posiciones, RGrid), /* intercambio cada posicion por un 0 en la grilla */