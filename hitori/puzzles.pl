% EXAMPLE PUZZELS. To get more, go to http://www.menneske.no/hitori/eng/
% puzzle(Num,Size,P) means that the puzzle with identifier Num has size Size (i.e., it is a Size by Size grid) 
% And that its representation (as a list of lists) is given by P. This means
%  P[i][j] is the number in the i'th row, j'th column





% puzzle 1, very easy, small
% http://www.menneske.no/hitori/5x5/eng/solve.html?number=8890
% solution: http://www.menneske.no/hitori/5x5/eng/solution.html?number=8890

puzzle(1,5,P) :-
	P=[ 
		[5, 3, 2, 2, 4],
		[4, 1, 5, 1, 3],
		[1, 3, 3, 5, 2],
		[4, 5, 2, 2, 1],
		[2, 2, 2, 1, 5]
	].


% puzzle 2, pretty easy, small
% http://www.menneske.no/hitori/5x5/eng/solve.html?number=21811
% solution: http://www.menneske.no/hitori/5x5/eng/solution.html?number=21811

puzzle(2,5,P) :-
	P=[ 
		[2, 4, 1, 2, 1],
		[1, 2, 3, 2, 5],
		[2, 3, 4, 4, 2],
		[2, 5, 1, 4, 3],
		[1, 1, 2, 3, 4]
	].

% puzzle 3,  easy, small
% http://www.menneske.no/hitori/5x5/eng/solve.html?number=21588
% solution: http://www.menneske.no/hitori/5x5/eng/solution.html?number=21588

puzzle(3,5,P) :-
	P=[ 
		[2, 3, 2, 4, 4],
		[3, 5, 4, 2, 2],
		[2, 2, 1, 5, 1],
		[2, 1, 5, 2, 4],
		[1, 4, 1, 2, 5]
	].



% puzzle 4,  normal, small
% http://www.menneske.no/hitori/5x5/eng/solve.html?number=13090
% solution: http://www.menneske.no/hitori/5x5/eng/solution.html?number=13090
puzzle(4,5,P) :-
	P=[ 
		[1, 5, 1, 3, 3],
		[4, 4, 3, 5, 2],
		[5, 4, 1, 2, 3],
		[2, 3, 2, 3, 1],
		[3, 2, 4, 1, 1]
	].


% puzzle 5,  prety easy, regular
% http://www.menneske.no/hitori/eng/showpuzzle.html?number=5702
% solution: http://www.menneske.no/hitori/eng/solution.html?number=5702
puzzle(5,9,P) :-
	P=[ 
		[7, 9, 4, 1, 1, 5, 9, 6, 8],
		[9, 9, 8, 4, 3, 1, 2, 8, 5],
		[3, 2, 1, 8, 1, 9, 3, 1, 4],
		[6, 3, 1, 6, 5, 8, 6, 4, 7],
		[6, 1, 5, 5, 7, 4, 2, 2, 8],
		[4, 7, 5, 2, 2, 6, 4, 9, 1],
		[5, 1, 6, 1, 9, 2, 3, 3, 3],
		[4, 5, 2, 6, 1, 7, 8, 7, 9],
		[1, 9, 4, 2, 8, 8, 7, 5, 6]
	].

% puzzle 6,  medium, regular
% http://www.menneske.no/hitori/5x5/eng/solve.html?number=13491
% solution: http://www.menneske.no/hitori/eng/solution.html?number=13491
puzzle(6,9,P) :-
	P=[ 
		[8, 9, 6, 2, 4, 5, 7, 4, 4],
		[4, 5, 4, 3, 6, 8, 1, 6, 2],
		[7, 6, 8, 3, 7, 6, 2, 5, 4],
		[1, 8, 2, 8, 7, 7, 4, 2, 5],
		[8, 7, 5, 8, 1, 6, 3, 2, 6],
		[6, 1, 4, 5, 2, 1, 8, 7, 3],
		[5, 1, 3, 1, 8, 4, 6, 6, 7],
		[7, 3, 1, 9, 4, 3, 5, 8, 3],
		[2, 4, 9, 7, 8, 2, 6, 7, 8]
	].

% puzzle 7, very hard, regular
% http://www.menneske.no/hitori/eng/showpuzzle.html?number=200564
% solution: http://www.menneske.no/hitori/eng/solution.html?number=200564

puzzle(7,9,P) :-
	P=[ 
		[3, 7, 6, 7, 4, 1, 5, 4, 2],
		[7, 2, 7, 1, 8, 5, 1, 4, 6],
		[9, 1, 3, 6, 2, 8, 7, 8, 5],
		[1, 8, 1, 3, 1, 2, 2, 6, 4],
		[6, 5, 2, 7, 3, 9, 1, 8, 1],
		[1, 5, 7, 4, 6, 3, 1, 2, 9],
		[4, 6, 4, 9, 8, 9, 2, 2, 3],
		[8, 9, 3, 8, 5, 6, 4, 7, 1],
		[1, 3, 2, 7, 6, 4, 6, 5, 3]
	].

% puzzle 8, super easy, large
% http://www.menneske.no/hitori/eng/showpuzzle.html?number=1769
% solution: http://www.menneske.no/hitori/eng/solution.html?number=1769
puzzle(8,17,P) :-
	P=[ 
		[16, 13, 15, 9, 14, 17, 5, 12, 9, 3, 15, 2, 6, 11, 15, 1, 6],
		[3, 6, 5, 7, 8, 15, 16, 1, 1, 8, 13, 10, 12, 12, 17, 4, 2],
		[5, 12, 1, 2, 13, 4, 3, 15, 9, 14, 8, 6, 16, 17, 3, 11, 7],
		[10, 14, 7, 8, 7, 11, 1, 11, 14, 12, 5, 3, 16, 5, 1, 9, 13],
		[6, 17, 3, 3, 16, 13, 10, 2, 6, 16, 9, 14, 15, 8, 12, 15, 13],
		[12, 1, 15, 17, 2, 5, 13, 6, 14, 8, 10, 9, 11, 15, 1, 16, 5],
		[14, 8, 11, 7, 3, 9, 15, 10, 5, 15, 11, 6, 4, 6, 13, 12, 4],
		[17, 1, 13, 1, 8, 7, 13, 3, 8, 9, 12, 8, 12, 4, 2, 15, 11],
		[7, 15, 8, 9, 9, 16, 11, 1, 4, 3, 3, 10, 13, 2, 1, 6, 14],
		[16, 7, 5, 15, 1, 10, 6, 14, 9, 6, 4, 5, 2, 15, 9, 8, 17],
		[6, 10, 12, 5, 11, 8, 17, 5, 3, 13, 16, 6, 1, 15, 13, 10, 9],
		[6, 9, 2, 3, 1, 1, 8, 4, 13, 17, 6, 16, 1, 3, 13, 14, 7],
		[1, 14, 11, 13, 5, 12, 9, 2, 11, 7, 16, 10, 8, 16, 15, 13, 9],
		[9, 12, 15, 16, 5, 14, 4, 14, 2, 16, 10, 13, 17, 16, 5, 3, 4],
		[13, 11, 14, 9, 4, 15, 13, 13, 17, 16, 1, 15, 3, 9, 10, 2, 10],
		[4, 1, 9, 3, 17, 15, 2, 10, 2, 15, 15, 12, 3, 14, 6, 13, 13],
		[4, 16, 11, 4, 2, 6, 14, 11, 10, 1, 2, 7, 9, 12, 5, 5, 3]
	].

% Normal size, super hard
% http://www.menneske.no/hitori/eng/showpuzzle.html?number=192728
% SOLUTION: http://www.menneske.no/hitori/eng/solution.html?number=192728

puzzle(9,9,P) :-
	P=[
		 [1, 5, 1, 8, 6, 7, 6, 2, 4],
		 [6, 9, 2, 3, 4, 5, 3, 7, 7],
		 [9, 1, 4, 2, 5, 2, 7, 7, 8],
		 [1, 2, 2, 1, 8, 3, 5, 6, 7],
		 [2, 3, 5, 7, 8, 4, 1, 8, 4],
		 [1, 8, 3, 4, 3, 1, 9, 1, 6],
		 [7, 1, 9, 5, 4, 6, 8, 3, 2],
		 [1, 4, 1, 3, 7, 8, 5, 9, 5],
		 [1, 7, 8, 9, 1, 3, 4, 1, 2]
	].

% Normal size, super hard
% http://www.menneske.no/hitori/eng/showpuzzle.html?number=200643
% SOLUTION: http://www.menneske.no/hitori/eng/solution.html?number=200643


puzzle(10,9,P) :-
	P=[
		 [1, 7, 8, 2, 4, 5, 2, 3, 7],
		 [6, 4, 9, 7, 4, 1, 2, 8, 6],
		 [8, 2, 7, 2, 9, 6, 4, 4, 3],
		 [2, 9, 1, 1, 3, 8, 3, 6, 4],
		 [7, 1, 4, 8, 6, 2, 5, 9, 1],
		 [2, 3, 1, 8, 7, 9, 6, 4, 4],
		 [4, 1, 8, 3, 1, 7, 9, 8, 4],
		 [6, 2, 3, 1, 1, 9, 5, 7, 2],
		 [9, 8, 5, 4, 8, 3, 1, 8, 7]
	].



% Normal size, super hard
% http://www.menneske.no/hitori/eng/showpuzzle.html?number=217712
% SOLUTION: http://www.menneske.no/hitori/eng/solution.html?number=217712

puzzle(11,9,P) :-
	P=[
		 [6, 6, 3, 8, 4, 9, 7, 8, 5],
		 [7, 5, 4, 9, 1, 3, 8, 2, 4],
		 [4, 1, 4, 7, 9, 2, 8, 8, 3],
		 [8, 8, 9, 3, 9, 2, 4, 6, 5],
		 [3, 4, 8, 2, 5, 8, 6, 8, 9],
		 [4, 2, 7, 1, 2, 4, 1, 9, 5],
		 [5, 9, 2, 6, 8, 7, 3, 1, 1],
		 [4, 4, 5, 2, 6, 4, 9, 1, 2],
		 [9, 3, 7, 4, 2, 6, 1, 8, 7]
	].




% Normal size, super hard
% http://www.menneske.no/hitori/eng/showpuzzle.html?number=236443
% SOLUTION: http://www.menneske.no/hitori/eng/solution.html?number=236443

puzzle(12,9,P) :-
	P=[
		 [8, 2, 9, 5, 8, 6, 8, 3, 1],
		 [5, 9, 1, 7, 1, 4, 3, 5, 6],
		 [4, 2, 7, 2, 8, 2, 5, 5, 6],
		 [1, 6, 4, 8, 9, 1, 2, 7, 5],
		 [5, 4, 8, 2, 7, 7, 6, 7, 3],
		 [3, 6, 5, 2, 6, 9, 1, 4, 9],
		 [1, 5, 3, 6, 3, 3, 2, 8, 2],
		 [9, 3, 2, 1, 5, 3, 7, 3, 4],
		 [6, 1, 1, 3, 3, 5, 8, 2, 9]
	].




% Normal size, super hard
% http://www.menneske.no/hitori/eng/showpuzzle.html?number=184108
% SOLUTION: http://www.menneske.no/hitori/eng/solution.html?number=184108

puzzle(13,9,P) :-
	P=[
		 [8, 6, 9, 7, 2, 4, 2, 1, 3],
		 [4, 1, 8, 2, 4, 8, 2, 5, 8],
		 [5, 9, 3, 6, 9, 1, 4, 7, 2],
		 [4, 3, 1, 5, 2, 1, 1, 6, 8],
		 [6, 8, 2, 6, 1, 5, 7, 5, 4],
		 [2, 7, 8, 4, 2, 6, 5, 9, 6],
		 [1, 4, 5, 8, 4, 9, 4, 2, 3],
		 [2, 6, 8, 3, 7, 6, 9, 4, 1],
		 [6, 2, 4, 2, 6, 3, 5, 8, 3]
	].





% Normal size, super hard
% http://www.menneske.no/hitori/eng/showpuzzle.html?number=234799
% SOLUTION: http://www.menneske.no/hitori/eng/solution.html?number=234799

puzzle(14,9,P) :-
	P=[
		 [8, 2, 6, 8, 4, 1, 9, 9, 5],
		 [4, 5, 4, 2, 6, 3, 8, 1, 8],
		 [9, 7, 8, 9, 5, 6, 3, 4, 3],
		 [9, 4, 1, 7, 1, 6, 2, 8, 9],
		 [1, 9, 5, 4, 4, 2, 2, 3, 7],
		 [6, 1, 7, 4, 9, 8, 5, 2, 4],
		 [1, 6, 1, 3, 5, 9, 7, 3, 4],
		 [3, 3, 9, 6, 2, 8, 6, 7, 8],
		 [4, 8, 2, 5, 9, 7, 9, 6, 9]
	].




% Large, impossible
% http://www.menneske.no/hitori/15x15/eng/showpuzzle.html?number=1963
% SOLUTION: http://www.menneske.no/hitori/15x15/eng/solution.html?number=1963


puzzle(15,15,P) :-
	P=[
		 [4, 7, 15, 10, 10, 12, 4, 9, 13, 1, 7, 3, 1, 14, 11],
		 [5, 3, 3, 1, 13, 6, 12, 5, 4, 4, 6, 14, 9, 11, 2],
		 [1, 8, 14, 15, 2, 9, 6, 6, 3, 7, 11, 5, 3, 10, 4],
		 [12, 5, 4, 9, 11, 1, 7, 3, 15, 3, 9, 6, 8, 1, 14],
		 [8, 13, 6, 11, 12, 6, 10, 4, 2, 15, 9, 4, 1, 2, 3],
		 [14, 3, 10, 1, 1, 13, 4, 2, 11, 9, 6, 8, 4, 3, 13],
		 [13, 14, 1, 12, 10, 3, 8, 4, 1, 3, 7, 1, 2, 1, 9],
		 [9, 9, 5, 1, 15, 4, 6, 8, 1, 2, 13, 7, 9, 12, 11],
		 [2, 6, 2, 11, 4, 11, 9, 1, 10, 13, 7, 12, 7, 1, 11],
		 [7, 3, 4, 2, 6, 8, 3, 12, 4, 11, 14, 2, 15, 10, 13],
		 [11, 12, 8, 3, 3, 6, 1, 10, 2, 2, 1, 9, 14, 7, 15],
		 [13, 4, 13, 3, 8, 15, 1, 3, 7, 12, 1, 11, 6, 8, 10],
		 [9, 9, 7, 2, 6, 5, 15, 14, 8, 14, 10, 9, 12, 1, 1],
		 [12, 11, 1, 5, 3, 2, 5, 13, 9, 4, 3, 10, 6, 15, 8],
		 [9, 2, 9, 11, 14, 7, 11, 12, 12, 3, 15, 9, 10, 3, 4]
	].





