-module(khf1).
-compile(export_all).
% -export([feldarabolasa/2]).

-type matrix() :: [row()].
-type row() :: [any()].
-type parameter() :: {subRows(), subCols()}.
-type subRows() :: integer().
-type subCols() :: integer().

% -spec khf1:feldarabolasa(Mss :: matrix(), P :: parameter()) -> Lss :: [[any()]].
% % feldarabolasa(Mss,P) ->
%     {X,_}=P, bigjump(Mss,P,length(Mss) div X).
% feldarabolasa(Mss,P) when length(Mss) rem element(1,P) == 0 ->
%     %last row check
%     %get to new 
%     [[]];
% feldarabolasa(Mss,P) when length(Mss) rem element(2,P) == 0 ->
%     %last column check
%     %get to new row
%     1.
% % feldarabolasa(Mss,P) ->

% bigcol(L,P,Count) when length(L) == 1 ->
%     {_,Y}=P,[subrow(lists:sublist(L,Y),P)];
% bigcol(Mss,P,Count) ->
%     {_,Y}=P,[subrow(Mss,P,Count) | bigcol(tl--bad(Mss),P)].

%jump number of subrows
% bigjump(Mss,P,0) ->
%    {X,_}=P, bigcol(Mss,P,0);
% bigjump(Mss,P,Count) ->
%     {X,_}=P, [bigcol(lists:sublist(Mss,X),P,Count) | bigjump(lists:nthtail(Mss,X*Count-1),P,Count-1)].

mtx() ->
    [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]].
p() ->
    {2,2}.

% colJump(Mss,P,0) -> 
%     [];
% colJump(Mss,P,Count) ->
%     [1 | colJump(Mss,P,Count-1)].
% rowJump(Mss,P,0) ->
%     % [colJump(Mss,P)];
%     [];
% rowJump(Mss,P,Norows) when (Norows rem element(1,P)) == 0 ->
%     % RT=rowJump(Mss,P,Norows-1),[colJump(Mss,P,4)] ++ [RT];
% subcol([],P,T) ->
%     [];
% subcol(L,P,T)

% subrow([],P,T) ->
%     [];
% subrow(L,P,T) ->
%     subrow(tl(L),P,T) ++ subcol(lists).

% subMtx(Mss,P,T) ->
%     {X,Y}=P,{A,B}=T,
%     subrow(lists:sublist(Mss,length(Mss)-A+X),P,T).
% colJump(Mss,P,T) when element(2,T)-element(2,P) =< 0 ->
%     [subMtx(Mss,P,T)];
% colJump(Mss,P,T) ->
%     {X,Y}=T,{_,B}=P,CJ=colJump(Mss,P,{X,Y-B}), CJ ++ [subMtx(Mss,P,{X,Y})].

% rowJump(Mss,P,T) when element(1,T)-element(1,P) =< 0 ->
%     colJump(Mss,P,T);
% rowJump(Mss,P,T) ->
%     io:fwrite("~w",[T]),
%     {X,Y}=T,{A,_}=P,RJ=rowJump(Mss,P,{X-A,Y}), RJ ++ colJump(Mss,P,{X,Y}).

%khf1:rowJump(khf1:mtx(),khf1:p(),{4,4}).

% CJ=colJump(Mss,P,{X,Y-B})

% subMtx(Mss,P,T) ->
%     {X,Y}=P,{A,B}=T,
%     subrow(lists:sublist(Mss,length(Mss)-A+X),P,T).

% colJump(Mss,P,T) when element(2,T)-element(2,P) =< 0 ->
%     [subMtx(Mss,P,T)];
% colJump(Mss,P,T) ->
%     {X,Y}=T,{_,B}=P,
%     CJ=colJump(Mss,P,{X,Y-B}), 
%     CJ ++ [subMtx(Mss,P,{X,Y})].

% rowJump(Mss,P,T) when element(1,T)-element(1,P) =< 0 ->
%     colJump(Mss,P,T);
% rowJump(Mss,P,T) ->
%     {X,Y}=T,{A,_}=P,
%     TL=lists:nthtail(X,Mss),
%     HL=lists:sublist(Mss,length()),
%     RJ=rowJump(TL,P,{X-A,Y}), 
%     RJ ++ colJump(TL,P,{X,Y}).


col(L,P) when length(hd(L))-element(2,P) == 0 ->
    [lists:foldl(fun(X,Acc) -> X++Acc end, [],L)];
col(L,P) when length(hd(L)) div element(2,P) == 0   ->
    [lists:foldl(fun(X,Acc) -> X++Acc end, [],L)];
col(L,P) ->
    {_,Y}=P,
    [lists:foldl(    fun(LL,Acc) -> Acc++lists:sublist(LL,Y) end, [],L)] ++
    col(lists:map(fun(LL) -> lists:nthtail(Y,LL) end,L),P).

% row([],P) ->
%     [];
row(Mss,P) when length(Mss) div element(1,P) == 0 ->
     col(Mss,P);
row(Mss,P) ->
    {X,_}=P,
    col(lists:sublist(Mss,X),P) ++ row(lists:nthtail(X,Mss),P).
    % row(lists:nthtail(X,Mss),P) ++ col(lists:sublist(Mss,X),P) .

%khf1:row(khf1:mtx(),khf1:p()).