:- module(init, [ init/2 ]).

/**
 * init(-Grid, -NumOfColumns).
 * 
 * Predicado especificando la grilla inicial, que será mostrada al comienzo del juego, donde
 * Grid es una lista con los números que conforman la grilla, y NumOfColumns es la cantidad de columnas, 
 * determinando las dimensiones de la misma.
 */

/* init([
	8,4,32,8,16,
	4,16,8,8,8,
	128,32,4,64,8,
	4,8,8,16,64,
	512,4,128,512,64,
	4,4096,1024,16,512,
	16,2048,128,64,2048,
	8,4096,32,8192,8
], 5). */

init([
	64,4,64,32,16,
	64,8,16,2,32,
	2,4,64,64,2,
	2,4,32,16,4,
	16,4,16,16,16,
	16,64,2,32,32,
	64,2,64,32,64,
	32,2,64,32,4
], 5).