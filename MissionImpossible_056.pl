:-["KB"].
goal(S):-(var(S), iterative_deepening(S,1));
	(nonvar(S),compound(S),reverse2(S,S1),ethan_loc(Ex,Ey),members_loc(T),create_agents_array_with_carry(T,T1),state(Ex,Ey,0,T1,S1),!).
	
state(X,Y,_,A,s0):- members_loc(T), create_agents_array_with_drop(T,A), submarine(X,Y).
state(Ex1,Ey1,Ec1,Agents,result(A,S)):-
	( (A=right, Ey is(Ey1+1), in_grid(Ex1,Ey), state(Ex1, Ey, Ec1,Agents, S));
	  (A=left,  Ey is(Ey1-1), in_grid(Ex1,Ey), state(Ex1, Ey, Ec1,Agents, S));
	  (A=up,    Ex is(Ex1-1), in_grid(Ex,Ey1), state(Ex, Ey1, Ec1,Agents, S));
	  (A=down,  Ex is(Ex1+1), in_grid(Ex,Ey1), state(Ex, Ey1, Ec1,Agents, S));
	  (A=carry, Ec is(Ec1+1), capacity(C), Ec=<C, carried(Ex1,Ey1,Agents,Agents1), state(Ex1, Ey1, Ec, Agents1, S));
	  (A=drop,  Ec is(Ec1- Ec1), submarine(Ex1,Ey1),dropped(Ex1,Ey1,Agents,Agents1), state(Ex1, Ey1, Ec, Agents1, S))).

in_grid(X,Y):-X>=0,X=<3,Y>=0,Y=<3.

carried(X,Y,[[X,Y,0]|T], [[X,Y,1]|T]).
carried(X1,Y1,[H|T],[H|T1]):-carried(X1,Y1,T,T1).

dropped(_,_,[[X,Y,1]|T], [[X,Y,2]|T]).
dropped(X1,Y1,[H|T],[H|T1]):-dropped(X1,Y1,T,T1).

create_agents_array_with_carry([],[]).
create_agents_array_with_carry([[X,Y]|T],[[X,Y,0]|T1]):-create_agents_array_with_carry(T,T1).

create_agents_array_with_drop([],[]).
create_agents_array_with_drop([[X,Y]|T],[[X,Y,2]|T1]):-create_agents_array_with_drop(T,T1).

reverse2(L, R) :- reverse2_helper(L, s0, R).

reverse2_helper(s0, R, R).
reverse2_helper(result(A,S), Acc, R) :-nonvar(A),NewAcc = result(A,Acc),reverse2_helper(S, NewAcc, R).

iterative_deepening(Goal,Limit):- ethan_loc(Ex,Ey),members_loc(T),create_agents_array_with_carry(T,T1),
	call_with_depth_limit(state(Ex,Ey,0,T1,S1), Limit, R),(\+R = depth_limit_exceeded,reverse2(S1,Goal);
	(R = depth_limit_exceeded,Limit1 is Limit + 1,iterative_deepening(Goal, Limit1))).
	

