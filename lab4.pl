/*
Joseph Howard (jhoward4@binghamton.edu)
CS471 - Programming Languages
Lab4 due 9-25-2014
Date: 9-23-2014
*/

/*
Question 1:
Homoiconic describes a language where the program code is accessible in the same format as one
of the funcdamental data types of the language. Prolog is homoiconic as it is a sequence of terms
that can be read, calculated and manipulated. Reflective languages are languages where the program
can modify the structure and behavior of the program at runtime. Prolog is not fully reflective,
but it is close. It can reslove a problem itself using the rules and relationships available to
it, but it can not create new relationships based on the existing ones like a fully reflective
language can.
*/

/*
Question 3:
*/
listOfTerms([],Y,[]).
listOfTerms([X|Xs],Y,[Z|Zs]):- listOfTerms(Xs,Y,Zs), Z=..[Y|X].
/*
Question 6
*/

insert(A,B,C):-select(A,X,B), X=C.

/*
Question 7
a)
The base case for convertToDecimal states that any value that can be unioned with variable A should
return 0 for the second argument. succ(succ(0)) falls into this catagory, it can be unioned with
variable A, therefore it returns 0. Any item that can union with A will return output 0.
b)
To fix it you simply need to narrow down what the base case is searching for. All successor notation
is built off of 0 as the primary building block, so search for the convertToDecimal(0,0) as the 
base case.
*/
numeral(0).
numeral(succ(X)):-numeral(X).

convertToDeciaml(A,0).
convertToDecimal(succ(S), Y):-numeral(S),Y1 is Y-1,convertToDecimal(S,Y1).

/*
Question 9
*/
swap(leaf(A), T):- T = leaf(A).
swap(tree(A,B),T):- T = tree(C,D), swap(B,C), swap(A,D).

/*
Question 10
I feel like equivT(A,A).
*/
equivT(leaf(A), T) :- T = leaf(A).
equivT(tree(A,B),T):- T = tree(C,D), equivT(A,C), equivT(B,D).

/*
Question 11
The not argument reverses the return of a statement. If the statement was true, it returns false,
and if it was false it returns true. Here we have A=apple, which returns true because A can be
unified with apple. The first not changes this to statement to false. The second not changes that
false to true. The actual query being passed is (A=B, true, B=what). Because A can also be unified
with 'what' the whole query returns true.
*/

/*
Question 12
*/
solv([D,E,M,N,O,R,S,Y]):-
	Lst = [S,E,N,D,M,O,R,Y],
	Digits = [0,1,2,3,4,5,6,7,8,9],
	assign_digits(Lst, Digits),
	M > 0,
	S > 0,
	1000*S + 100*E + 10*N + D +
	1000*M + 100*O + 10*R + E =:=
	10000*M + 1000*O + 100*N + 10*E + Y.

assign_digits([], _List).
assign_digits([D|Ds], List):-
        select(D, List, NewList),
        assign_digits(Ds, NewList).
