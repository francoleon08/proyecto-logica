:- module(init, [ init/2 ]).

/**
 * init(-Grid, -NumOfColumns).
 * 
 * Predicado especificando la grilla inicial, que será mostrada al comienzo del juego, donde
 * Grid es una lista con los números que conforman la grilla, y NumOfColumns es la cantidad de columnas, 
 * determinando las dimensiones de la misma.
 */

/* init([
	2,2,2,2,2,
	2,2,2,2,2,
	2,2,2,2,2,
	2,2,2,2,2
], 5). */

init([4,4,64,32,16,
	 4,16,16,2,32,
	 2,4,64,64,2,
	 2,4,32,16,4,
	 16,4,16,16,16,
	 16,64,2,32,32,
	 64,2,64,32,64,
	 32,2,64,32,4
	 ], 5).

/* Grilla Ivan Test */
/* init([
	2,2,64,8,16,
	2,4,32,2,32,
	2,8,2,2,2,
	2,16,2,8,4,
	2,4,2,4,2,
	2,16,2,8,2,
	2,8,2,4,2,
	4,2,4,2,4
], 5). */


/* init([
	4,2,2,8,2,
	2,4,2,4,2
], 5). */