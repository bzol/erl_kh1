-module(khf1).
-compile(export_all).
% -export([feldarabolasa/2]).

-type matrix() :: [row()].
-type row() :: [any()].
-type parameter() :: {subRows(), subCols()}.
-type subRows() :: integer().
-type subCols() :: integer().

-spec khf1:feldarabolasa(Mss :: matrix(), P :: parameter()) -> Lss :: [[any()]].
feldarabolasa(Mss,P) ->
    {X,_}=P, bigjump(Mss,P,length(Mss) div X).
% feldarabolasa(Mss,P) when length(Mss) rem element(1,P) == 0 ->
%     %last row check
%     %get to new 
%     [[]];
% feldarabolasa(Mss,P) when length(Mss) rem element(2,P) == 0 ->
%     %last column check
%     %get to new row
%     1.
% % feldarabolasa(Mss,P) ->
subrow(L,Y) ->
    void.


bigcol(L,P) when length(L) == 1 ->
    {_,Y}=P,[subrow(lists:sublist(L,Y),P)];
bigcol(L,P) ->
    {_,Y}=P,[subrow(hd(L),P) | bigcol(tl(L),P)].

%jump number of subrows
bigjump(Mss,P,0) ->
   {X,_}=P, bigcol(Mss,P);
bigjump(Mss,P,Count) ->
    {X,_}=P, [bigcol(lists:sublist(Mss,X),P) | bigjump(lists:nthtail(Mss,X*Count-1),P,Count-1)].

mtx() ->
    [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]].