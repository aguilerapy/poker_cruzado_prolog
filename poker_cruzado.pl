% Poker Cruzado (25 cartas de 28)
% Los símbolos que se utilizarán para representar a:
%    • jugada: pareja, doblePareja, trío, full, poker,
%    escalera.
%    • valor de carta: 8, 9, 10, j, q, k, as.

% Se incluye el archivo de pistas segun requerimiento
:- ['archivo_pistas.pl'].

% Predicado principal que soluciona el problema del Poker Cruzado
% El resultado lo coloca en la matriz M
% M = solucion(fila1(), fila2(), fila3(), fila4(), fila5())
poker_cruzado() :-
        crear_lista(5, 0, R),
        lista(R, 5, C),
        M =.. [inicio | C],
        cargar_pistas(1,1,M),
        duplicate_term(M, M_inicio),
        solucionar(M, M_inicio).

% Busca la solucion y muestra en pantalla
solucionar(M, N) :-
        buscarsolucion(M,M4),
        M4 =.. M3,
        [_|S] = M3,
        M2 =.. [solucion | S],
        nl, write(N), nl, nl,
        write("Matriz inicio:"), nl, nl,
        muestraMatriz(1,1,N),
        write(M2), nl, nl,
        write("Matriz solucion:"), nl, nl,
        muestraMatriz(1,1,M).

% Busqueda de una solucion controlando todas las posibilidades
buscarsolucion(M, M4) :-
        findnsols(1,M,llenar_matriz(M),[M4|_]).

% Encontrar Una Solucion, Llenando La Matriz Con Distintas Opciones
llenar_matriz(M) :-
        llenar_fila(M,1),
        llenar_fila(M,2),
        llenar_fila(M,3),
        llenar_fila(M,4),
        llenar_fila(M,5),
        controlar_columna(M,1),
        controlar_columna(M,2),
        controlar_columna(M,3),
        controlar_columna(M,4),
        controlar_columna(M,5),
        controlar_diagonal(M).

% Control de columna
controlar_columna(M, Y) :-
        get_cell(1, Y, M, A),
        get_cell(2, Y, M, B),
        get_cell(3, Y, M, C),
        get_cell(4, Y, M, D),
        get_cell(5, Y, M, E),
        determinar_mano([A,B,C,D,E], Jugada),
        call(columna, Y, Jugada).

% Control de diagonal
controlar_diagonal(M) :-
        get_cell(1, 1, M, A1),
        get_cell(2, 2, M, B1),
        get_cell(3, 3, M, C1),
        get_cell(4, 4, M, D1),
        get_cell(5, 5, M, E1),
        get_cell(1, 5, M, A2),
        get_cell(2, 4, M, B2),
        get_cell(3, 3, M, C2),
        get_cell(4, 2, M, D2),
        get_cell(5, 1, M, E2),
        determinar_mano([A1,B1,C1,D1,E1], Jugada1),
        determinar_mano([A2,B2,C2,D2,E2], Jugada2),
        call(diagonal1, Jugada1),
        call(diagonal2, Jugada2).

% Encontrar una solucion para la fila N, con Pista
llenar_fila(M, X) :-
        findall(Ya,get_cell(X, Ya, M, 0),Y),
        cargar_carta_lista(X, Y, M),
        findall(F,arg(X,M,F),[F1|_]),
        F1 =.. F2,
        [_|Mano] = F2,
        determinar_mano(Mano, Jugada),
        call(fila, X, Jugada).

% Cargar la carta en la matriz
cargar_carta_lista(_, [], _).
cargar_carta_lista(X, [C|L], M) :-
        cargar_carta(X, C, M),
        cargar_carta_lista(X, L, M).
cargar_carta(X, Y, M) :-
        random(8,15,Carta),
        valor_carta(Carta,Carta2),
        %random_between(8,14,Carta),
        %findnsols(1,Carta,valor_carta(_, Carta),Carta2),
        %valor_carta(Carta, Carta2),
        set_cell(X,Y,M,Carta2).

% Cambiar 11,12,13,14 por j,q,k,a
valor_carta(11, j).
valor_carta(12, q).
valor_carta(13, k).
valor_carta(14, a).
valor_carta(X, Y) :-  X < 11, X > 7, Y = X.

% Determinar que mano corresponde a la columna o diagonal para controlar
% con las pistas
determinar_mano([A,B,C,D,E], pareja) :- A = B; A = C; A = D; A = E;
                                        B = C; B = D; B = E;
                                        C = D; C = E;
                                        D = E.

determinar_mano([A,B,C,D,E], doblePareja) :- (B = C, D = E); (B = D, C = E); (B = E, C = D);
                                             (A = C, D = E); (A = D, C = E); (A = E, C = D);
                                             (A = B, D = E); (A = D, B = E); (A = E, B = D);
                                             (A = B, C = E); (A = C, B = E); (A = E, B = C);
                                             (A = B, C = D); (A = C, B = D); (A = D, B = C).

determinar_mano([A,B,C,D,E], trio) :- (A = B, B = C); (A = B, B = D); (A = B, B = E);
                                      (B = C, C = D); (B = C, C = E); (B = D, D = E);
                                      (A = C, C = D); (A = C, C = E); (A = D, D = E); (C = D, D = E).

determinar_mano([A,B,C,D,E], full) :- (A = B, B = C, D = E); (A = B, B = D, C = E); (A = B, B = E, C = D);
                                      (B = C, C = D, A = E); (B = C, C = E, A = D); (B = D, D = E, A = C );
                                      (A = C, C = D, B = E); (A = C, C = E, B = D); (A = D, D = E, B = C); (C = D, D = E, A = B).

determinar_mano([A,A,A,A,_], poker).
determinar_mano([A,A,A,_,A], poker).
determinar_mano([A,A,_,A,A], poker).
determinar_mano([A,_,A,A,A], poker).
determinar_mano([_,A,A,A,A], poker).

determinar_mano(L, escalera) :- (verificar_repetidos(L), not(member(k,L)), not(member(a,L)));
                                (verificar_repetidos(L), not(member(8,L)), not(member(a,L)));
                                (verificar_repetidos(L), not(member(8,L)), not(member(9,L)));
                                (verificar_repetidos(L), not(member(q,L)), not(member(k,L))).

determinar_mano(_, nada).

% Verificar que todos las cartas sean diferentes
verificar_repetidos([A,B,C,D,E]) :-
        F = [A,B,C,D,E],
        contar(A,F,C1),
        contar(B,F,C2),
        contar(C,F,C3),
        contar(D,F,C4),
        contar(E,F,C5),
        C1 = C2, C2 = C3, C3 = C4, C4 = C5, C5 = 0.

% Predicados auxiliares
contar(_,[],-1).
contar(X,[X|L],C):- !, contar(X,L,C1), C is C1+1.
contar(X,[_|L],C):- contar(X,L,C).

% Cargar pistas en M
cargar_pistas(6,_,_).
cargar_pistas(_,6,_).
cargar_pistas(X, Y, M) :-
        cargar_carta_pista(X,Y,M),
        rompeLineax(Y,X,X2),
        dezplazar(Y,Y2),
        cargar_pistas(X2, Y2, M).

% Cargar la carta que dice la pista
cargar_carta_pista(X,Y,M) :-
        call(carta,X,Y,A),
        set_cell(X,Y,M,A).
cargar_carta_pista(_,_,_).

% Devuelve el valor de la posicion (X, Y) de la matriz M.
get_cell(X, Y, M, Val) :-
        arg(X, M, F), % Devuelve en F la Row X
        arg(Y, F, Val). % Devuelve en Val el elemento de la columna Y de F

% Setea el valor de la posicion (X, Y) de la matriz M.
set_cell(X, Y, M, Val) :-
        arg(X, M, F),
        setarg(Y, F, Val).

% Devuelve en R una fila de largo Y, con todos los valores seteados en Val.
crear_lista(Y, Val, R) :-
        lista(Val, Y, L),
        R =.. [fila | L].

% Devuelve en L una lista de elementos igual a Val, de largo N
lista(_, 0, []).
lista(Val, N, L) :-
        N > 0,
        M is N - 1,
        duplicate_term(Val, NewVal),
        L = [Val| X],
        lista(NewVal, M, X).

% Mostrar la solucion
muestraMatriz(6,_,_).
muestraMatriz(_,6,_).
muestraMatriz(X, Y, M):-
        get_cell(X, Y, M, A),
        write(A),
        espacio(A),
        rompeLinea(Y,X,X2),
        dezplazar(Y,Y2),
        muestraMatriz(X2,Y2,M).
espacio(10):- write("  ").
espacio(_) :- write("   ").
rompeLinea(5,X,X2) :-
        dezplazarx(X,X2),
        nl,
        nl.
rompeLinea(_,X,X).
rompeLineax(5,X,X2) :-
        dezplazarx(X,X2).
rompeLineax(_,X,X).
dezplazar(1,2).
dezplazar(2,3).
dezplazar(3,4).
dezplazar(4,5).
dezplazar(5,1).
dezplazarx(1,2).
dezplazarx(2,3).
dezplazarx(3,4).
dezplazarx(4,5).
dezplazarx(5,6).

