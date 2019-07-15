% Archivo de Pistas para Poker Cruzado
% Existe una carta en la fila i, columna j de la matriz, cuyo valor es v
% carta(i,j,v).
carta(1,1,a).
carta(1,5,10).
carta(2,1,a).
carta(3,3,8).
carta(4,2,9).
carta(5,2,q).
carta(5,4,q).

% En la fila i de la matriz hay una mano clasificada como jugada
% fila(i, jugada).
fila(1,escalera).
fila(2,nada).
fila(3,pareja).
fila(4,doblePareja).
fila(5,full).

% En la columna j de la matriz hay una mano clasificada como jugada
% columna(j, jugada).
columna(1,pareja).
columna(2,nada).
columna(3,trio).
columna(4,pareja).
columna(5,escalera).

% En la diagonal con las posiciones (1,1) a (5,5) de la matriz hay una mano clasificada como doble pareja
diagonal1(doblePareja).

% En la diagonal con las posiciones (5,1) a (1,5) de la matriz hay una mano clasificada como póker
diagonal2(nada).
