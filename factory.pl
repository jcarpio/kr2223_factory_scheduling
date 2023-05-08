/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Simsttab -- Simplistic school time tabler
   Copyright (C) 2005-2022 Markus Triska triska@metalevel.at
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   For more information about this program, visit:
          https://www.metalevel.at/simsttab/
          ==================================
   Tested with Scryer Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- load_files('req_factory.pl').

:- use_module(library(clpfd)).
:- use_module(library(persistency)).
:- use_module(library(reif)).

/*
:- dynamic(tool_task_worker_times/4).
:- dynamic(coupling/4).
:- dynamic(teacher_freeday/2).
:- dynamic(slots_per_day/1).
:- dynamic(slots_per_week/1).
:- dynamic(tool_freeslot/2).
:- dynamic(room_alloc/4)
*/

:- dynamic tool_task_worker_times/4.
:- dynamic coupling/4.
:- dynamic teacher_freeday/2.
:- dynamic slots_per_day/1.
:- dynamic slots_couplings/2.
:- dynamic slots_per_week/1.
:- dynamic tool_freeslot/2.
:- dynamic room_alloc/4.


:- discontiguous tool_task_worker_times/4.
:- discontiguous tool_freeslot/2.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			 Posting constraints
   The most important data structure in this CSP are pairs of the form
      Req-Vs
   where Req is a term of the form req(To,Ta,W,N) (see below), and Vs is
   a list of length N. The elements of Vs are finite domain variables
   that denote the *time slots* of the scheduled lessons of Req. We
   call this list of Req-Vs pairs the requirements.
   To break symmetry, the elements of Vs are constrained to be
   strictly ascending (it follows that they are all_different/1).
   Further, the time slots of each teacher are constrained to be
   all_different/1.
   For each requirement, the time slots divided by slots_per_day are
   constrained to be strictly ascending to enforce distinct days,
   except for coupled lessons.
   The time slots of each class, and of lessons occupying the same
   room, are constrained to be all_different/1.
   Labeling is performed on all slot variables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


tools(Tools) :-
       setof(to, Ta^W^N^tool_task_worker_times(To,Ta,W,N), Tools).
		
workers(Workers) :-
        setof(T, To^Ta^N^tool_task_worker_times(To,Ta,W,N), Workers).
		
rooms(Rooms) :-
        findall(Room, room_alloc(Room,_T,_Ta,_Slot), Rooms0),
        sort(Rooms0, Rooms).
		
requirements(Rs) :-
        Goal = tool_task_worker_times(Tool,Task,Worker,Number),
        setof(req(Tool,Task,Worker,Number), Goal, Rs0),
        maplist(req_with_slots, Rs0, Rs).

req_with_slots(R, R-Slots) :- R = req(_,_,_,N), length(Slots, N).


pairs_slots(Ps, Vs) :-
        pairs_values(Ps, Vs0),
        append(Vs0, Vs).
			
sameroom_var(Reqs, r(Tool,Task,Lesson), Var) :-
        memberchk(req(Tool,Task,_Worker,_Num)-Slots, Reqs),
        nth0(Lesson, Slots, Var).
		
strictly_ascending(Ls) :- chain(Ls, #<).

constrain_room(Reqs, Room) :-
        findall(r(Tool,Task,Less), room_alloc(Room,Tool,Task,Less), RReqs),
        maplist(sameroom_var(Reqs), RReqs, Roomvars),
        all_different(Roomvars).

slot_quotient(Ta, Q) :-
        slots_per_day(SPD),
        Q #= Ta // SPD.

/*		
without_([], _, Es) --> seq(Es).
without_([W|Ws], Pos0, [E|Es]) -->
        { Pos #= Pos0 + 1,
          zcompare(R, W, Pos0) },
        without_at_pos0(R, E, [W|Ws], Ws1),
        without_(Ws1, Pos, Es).
without_at_pos0(=, _, [_|Ws], Ws) --> [].
without_at_pos0(>, E, Ws0, Ws0) --> [E].
*/

% list_without_nths(Es0, Ws, Es) :-
%        phrase(without_(Ws, 0, Es0), Es).

 list_without_nths(List, [], List).
 
 list_without_nths(List, [Head|Tail], R2):-
   list_without_nths(List, Tail, R), elimina_pos(R, Head, R2).
   
/*   
 elimina_pos(+List, +Pos, -R)
   es cierto si R unifica con una lista que contiene los elementos
   de List exceptuando el que ocupa la posición Pos. Los
   valores de posiciones empiezan en 0.
*/

 elimina_pos([], _, []).
 
 elimina_pos([_|Tail], 0, Tail).
 
 elimina_pos([Head|Tail], Pos, [Head|R]):- Pos > 0, Pos2 #= Pos - 1,
   elimina_pos(Tail, Pos2, R).
		
%:- list_without_nths("abcd", [3], "abc").
%:- list_without_nths("abcd", [1,2], "ad").		
		
		
requirements_variables(Rs, Vars) :-
        requirements(Rs),
        pairs_slots(Rs, Vars),
        slots_per_week(SPW),
        Max #= SPW - 1,
        Vars ins 0..Max,
        maplist(constrain_task, Rs),
        tools(tools),
        workers(Workers),
        rooms(Rooms),
        maplist(constrain_worker(Rs), Workers),
        maplist(constrain_class(Rs), Tools),
        maplist(constrain_room(Rs), Rooms).

constrain_class(Rs, Tool) :-
        tfilter(tool_req(Tool), Rs, Sub),
        pairs_slots(Sub, Vs),
        all_different(Vs),
        findall(S, tool_freeslot(Tool,S), Frees),
        maplist(all_diff_from(Vs), Frees).

all_diff_from(Vs, F) :- maplist(#\=(F), Vs).

constrain_task(req(Tool,Task,_Worker,_Num)-Slots) :-
        strictly_ascending(Slots), % break symmetry
        maplist(slot_quotient, Slots, Qs0),
        findall(F-S, coupling(Tool,Task,F,S), Ts),
        maplist(slots_couplings(Slots), Ts),
        pairs_values(Ts, Seconds0),
        sort(Seconds0, Seconds),
        list_without_nths(Qs0, Seconds, Qs),
        strictly_ascending(Qs).	
		
slots_couplings(Slots, F-S) :-
        nth0(F, Slots, S1),
        nth0(S, Slots, S2),
        S2 #= S1 + 1.		

constrain_worker(Rs, Worker) :-
        tfilter(teacher_req(Worker), Rs, Sub),
        pairs_slots(Sub, Vs),
        all_different(Vs),
        findall(F, teacher_freeday(Worker, F), Fs),
        maplist(slot_quotient, Vs, Qs),
        maplist(all_diff_from(Qs), Fs).

teacher_req(T0, req(_T,_S,T1,_N)-_, T) :- =(T0,T1,T).
tool_req(T0, req(T1,_S,_T,_N)-_, T) :- =(T0, T1, T).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Relate teachers and classes to list of days.
   Each day is a list of tasks (for classes), and a list of
   class/task terms (for teachers). The predicate days_variables/2
   yields a list of days with the right dimensions, where each element
   is a free variable.
   We use the atom 'free' to denote a free slot, and the compound terms
   tool_task(T, S) and task(S) to denote classes/tasks.
   This clean symbolic distinction is used to support tasks
   that are called 'free', and to improve generality and efficiency.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

days_variables(Days, Vs) :-
        slots_per_week(SPW),
        slots_per_day(SPD),
        NumDays #= SPW // SPD,
        length(Days, NumDays),
        length(Day, SPD),
        maplist(same_length(Day), Days),
        append(Days, Vs).
		
tool_days(Rs, Tool, Days) :-
        days_variables(Days, Vs),
        tfilter(tool_req(Tool), Rs, Sub),
        foldl(v(Sub), Vs, 0, _).

v(Rs, V, N0, N) :-
        (   member(req(_,Task,_,_)-Times, Rs),
            member(N0, Times) -> V = task(Task)
        ;   V = free
        ),
        N #= N0 + 1.

teacher_days(Rs, Worker, Days) :-
        days_variables(Days, Vs),
        tfilter(teacher_req(Worker), Rs, Sub),
        foldl(v_worker(Sub), Vs, 0, _).

v_worker(Rs, V, N0, N) :-
        (   member(req(T,Task,_,_)-Times, Rs),
            member(N0, Times) -> V = tool_task(T, Task)
        ;   V = free
        ),
        N #= N0 + 1.
		
% requirements_variables(Rs, Vs), labeling([ff], Vs), tool_days(Rs, '1a', Days), transpose(Days, DaysT).

print_tools(Rs) :-
        classes(Ts),
        format_tools(Ts, Rs).

format_tools([], _).
format_tools([Tool|Tooles], Rs):-
  tool_days(Rs, Tool, Days0),
  transpose(Days0, Days),
  format("Tool: ~w~2n", [Tool]),
  weekdays_header,
  align_rows(Days),
  format_tools(Tooles, Rs).
  
% [task(mat), free, tool_task('1a', mat), free]
% [mat, '', '1a/mat', '']
 
align_rows([]):- format("\n\n\n",[]).
align_rows([R|Rs]):-
        align_row(R),
        format("\n",[]),
        align_rows(Rs).


align_row(Row):-
		translate_row(Row, R2),
		format("~t~w~t~8+~t~w~t~8+~t~w~t~8+~t~w~t~8+~t~w~t~8+", R2).
  
weekdays_header():-      		
		format("~t~w~t~8+~t~w~t~8+~t~w~t~8+~t~w~t~8+~t~w~t~8+", ['Mon', 'Tue','Wed','Thu','Fri']),
        format("~n~`=t~40|~n", []). 

translate_row([], []).
translate_row([task(S)|Tail], [S|R]):-   
   translate_row(Tail, R).
translate_row([tool_task(T,S)|Tail], [T/S|R]):-   
   translate_row(Tail, R).
translate_row([free|Tail], [''|R]):-   
   translate_row(Tail, R).


align(free):- format("~t~w~t~8+", ['']).
align(tool_task(T,S)):- format("~t~w~t~8+", [T/S]).		
align(task(S)):- format("~t~w~t~8+", [S]).
align([E1,E2,E3,E4,E5]):- format("~t~w~t~8+~t~w~t~8+~t~w~t~8+~t~w~t~8+~t~w~t~8+", [E1,E2,E3,E4,E5]).

with_verbatim(T, verbatim(T)).

format_workers([], _).
format_workers([T|Ts], Rs):-
        teacher_days(Rs, T, Days0),
        transpose(Days0, Days),
        format("Worker: ~w~2n", [T]),
        weekdays_header,
        align_rows(Days),
        format_workers(Ts, Rs).
		
print_workers(Rs) :-
        teachers(Ts),
        format_workers(Ts, Rs).		
		
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ?- requirements_variables(Rs, Vs), labeling([ff], Vs), print_tools(Rs).
   %@ Tool: 1a
   %@
   %@   Mon     Tue     Wed     Thu     Fri
   %@ ========================================
   %@   mat     mat     mat     mat     mat
   %@   eng     eng     eng
   %@    h       h
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
