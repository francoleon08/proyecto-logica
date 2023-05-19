:- module(init, [ init/2 ]).

/**
 * init(-Grid, -NumOfColumns).
 * 
 * Predicado especificando la grilla inicial, que será mostrada al comienzo del juego, donde
 * Grid es una lista con los números que conforman la grilla, y NumOfColumns es la cantidad de columnas, 
 * determinando las dimensiones de la misma.
 */

init([
	8,4,32,8,16,
	4,16,8,8,8,
	128,32,4,64,8,
	4,8,8,16,64,
	512,4,128,512,64,
	4,4096,1024,16,512,
	16,2048,128,64,2048,
	8,4096,32,8192,8
], 5).