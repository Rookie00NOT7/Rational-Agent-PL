:-["KB"].
goal(S):-
	ethan_loc(Ex,Ey),
	members_loc(T),
	length(T,L),
	create_agents_array_with_carry(T,T1),
	state(Ex,Ey,0,L,T1,S1),
	reverse2(S1,S).
state(X,Y,_,_,A,s0):-
	members_loc(T),
	create_agents_array_with_carry2(T,A),
	submarine(X,Y).
state(Ex1,Ey1,Ec1,Agentsleft1,Agents,result(A,S)):-

	( (A=right, Ey is(Ey1+1), ingrid(Ex1,Ey), state(Ex1, Ey, Ec1, Agentsleft1,Agents, S));
	  (A=left,  Ey is(Ey1-1), ingrid(Ex1,Ey), state(Ex1, Ey, Ec1, Agentsleft1,Agents, S));
	  (A=up,    Ex is(Ex1-1), ingrid(Ex,Ey1), state(Ex, Ey1, Ec1, Agentsleft1,Agents, S));
	  (A=down,  Ex is(Ex1+1), ingrid(Ex,Ey1), state(Ex, Ey1, Ec1, Agentsleft1,Agents, S));
	  (A=carry, Ec is(Ec1+1), capacity(C), Ec=<C, carried(Ex1,Ey1,Agents,Agents1), state(Ex1, Ey1, Ec, Agentsleft1, Agents1, S));
	  (A=drop,  Ec is(Ec1- Ec1), Agentsleft is(Agentsleft1 - Ec1), submarine(Ex1,Ey1),dropped(Ex1,Ey1,Agents,Agents1), state(Ex1, Ey1, Ec, Agentsleft, Agents1, S)));
	( \+A=right, \+A=left, \+A=up, \+A=down, \+A=carry, \+A=drop, state(Ex1,Ey1,Ec1,Agentsleft1,Agents,S)).
ingrid(X,Y):-
        X>=0,X=<3,Y>=0,Y=<3.

carried(X,Y,[[X,Y,0]|T], [[X,Y,1]|T]).
carried(X1,Y1,[H|T],[H|T1]):-carried(X1,Y1,T,T1).

dropped(_,_,[[X,Y,1]|T], [[X,Y,2]|T]).
dropped(X1,Y1,[H|T],[H|T1]):-dropped(X1,Y1,T,T1).


create_agents_array_with_carry([],[]).
create_agents_array_with_carry([[X,Y]|T],[[X,Y,0]|T1]):-create_agents_array_with_carry(T,T1).

create_agents_array_with_carry2([],[]).
create_agents_array_with_carry2([[X,Y]|T],[[X,Y,2]|T1]):-create_agents_array_with_carry2(T,T1).

reverse2(L, R) :-
	reverse1(L, s0, R).

reverse1(s0, R, R).
reverse1(result(A,S), Acc, R) :-
	NewAcc = result(A,Acc),
	reverse1(S, NewAcc, R).
