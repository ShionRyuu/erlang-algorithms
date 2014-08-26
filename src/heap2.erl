%%
%% %CopyrightBegin%
%%
%% Copyright © 2013-2014 Aggelos Giantsios
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
%% and associated documentation files (the “Software”), to deal in the Software without restriction, 
%% including without limitation the rights to use, copy, modify, merge, publish, distribute, 
%% sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included 
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
%% TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN 
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% %CopyrightEnd%
%%

%% @copyright 2013-2014 Aggelos Giantsios
%% @author Aggelos Giantsios

%% ============================================================================
%% @doc Min-Heap, Max-Heap for Priority Queues
%%
%% Slightly changed by Ryuu in Aug. 2014
%% Changes:
%%   1. external ets
%%   2. identifier for heap
%%   3. heap capacity
%%   4. user preference comparator
%%   5. data storage separation
%% TODO:
%%   bug fix
%%
-module(heap2).

-export([new/3, new/4, new/5, from_list/4, from_list/5, from_list/6]).
-export([heap_size/1, is_empty/1, max/1, min/1, delete/2, insert/3,
  take_min/1, take_max/1, update/3, to_list/1, get_values/1, is_valid/1,
  is_full/1, heap_capacity/1, insert_or_update/3, clear/1, delete_heap/1]).

-export_type([heap/0]).

-define(KEY(Key), wrap_key(Key)).
-define(GE(Val), Val >= 0).
-define(GT(Val), Val > 0).
-define(LE(Val), Val =< 0).
-define(LT(Val), Val < 0).

%%
%% @type heap(). Min / Max Heap.
%%
-record(heap, {
  id :: term(),
  mode :: mode(),
  htab :: ets:tab(),
  capacity :: capacity(),
  cmp :: function()
}).

-type heap() :: #heap{}.
-type mode() :: max | min.
-type kv_pair() :: {term(), term()}.
-type kv_list() :: [kv_pair()].
-type capacity() :: integer() | infinity.


%% =======================================================================
%% External Exports
%% =======================================================================

%% @doc Creates an empty heap.
%% <p>If <code>M</code> is <code>max</code> then it will be a max heap, 
%% else if <code>M</code> is <code>min</code> it will be a min heap.</p>

new(ID, E, M) ->
  new(ID, E, M, infinity).

new(ID, E, M, C) ->
  new(ID, E, M, C, fun cmp/2).

new(ID, E, M, C, Cmp) ->
  H = #heap{id = ID, mode = M, cmp = Cmp, capacity = C, htab = E},
  set_size(H, 0),
  set_capacity(H, C),
  H.

%% @doc Create a heap from a list of terms.
%% <p>It returns the heap and a list of tuples <code>{Key, Ref}</code>
%% where <code>Key</code> is the term that was added and <code>Ref</code>
%% is its reference (used to change its priority).</p>
%% -spec from_list(mode(), kv_list()) -> {heap(), [refterm()]}.

from_list(N, E, M, L) ->
  from_list(N, E, M, infinity, L).

from_list(N, E, M, C, L) ->
  from_list(N, E, M, C, fun cmp/2, L).

from_list(N, E, M, C, Cmp, L) when is_list(L), is_atom(M), length(L) =< C ->
  H = new(N, E, M, C, Cmp),
  HS = ets_from_elements(H, L),
  I = parent(HS),
  construct_heap(H, I, HS),
  H;
from_list(_N, _E, _M, _C, _Cmp, _L) ->
  erlang:error(badarg).

%% @doc Add a new element to a heap.
%% <p>It returns a tuple with the element added and a reference
%% so that one can change its priority.</p>
-spec insert(heap(), term(), term()) -> ok | {error, full}.

insert(H, Key, Val) ->
  case is_full(H) of
    true ->
      HS = heap_size(H),
      {RefTail, Tail} = get_element(H, HS),
      #heap{mode = Mode, cmp = Cmp} = H,
      case Cmp(Val, Tail) of
        CR when Mode =:= min, ?LT(CR); Mode =:= max, ?GT(CR) ->
          del_map(H, RefTail),
          up_heapify(H, HS, Val, Key);
        _ ->
          {error, full}
      end;
    _ ->
      HS = heap_size(H),
      HS_n = HS + 1,
      set_size(H, HS_n),
      up_heapify(H, HS_n, Val, Key)
  end.

%% @doc Change the priority of an element.
%% <p>It changes the priority of the element referenced with
%% <code>Ref</code> to <code>Value</code> and then re-arranges the heap.</p>
-spec update(heap(), term(), term()) -> ok.

update(H, Ref, X) ->
  case get_map(H, Ref) of
    [] ->
      ok;
    I ->
      update_h(H, I, Ref, X)
  end.

update_h(H, I, Ref, X) ->
  {Ref, OldX} = get_element(H, I),
  #heap{cmp = Cmp, mode = Mode} = H,
  case {Cmp(X, OldX), Mode} of
    {CR, max} when ?GT(CR) -> up_heapify(H, I, X, Ref);
    {CR, min} when ?LT(CR) -> up_heapify(H, I, X, Ref);
    {CR, max} when ?LT(CR) -> down_heapify(H, I, X, Ref);
    {CR, min} when ?GT(CR) -> down_heapify(H, I, X, Ref);
    _ -> ignore
  end,
  ok.

%% @doc Update a present element or insert a new element.
%% <p>If the element referenced with <code>Ref</code> is present,
%% Value will be updated, otherwise will try to insert a new element.</p>
%% <p>A better name is required.</p>
-spec insert_or_update(heap(), term(), term()) -> ok | {error, full}.

insert_or_update(H, Ref, Val) ->
  case get_map(H, Ref) of
    [] ->
      insert(H, Ref, Val);
    I ->
      update_h(H, I, Ref, Val)
  end.

%% @doc Deletes element from a heap.
-spec delete(heap(), term()) -> ok.

delete(H, Ref) ->
  case get_map(H, Ref) of
    [] ->
      ok;
    I ->
      HS = heap_size(H),
      {Ref_n, Val} = get_element(H, HS),
      del_element(H, I),
      del_map(H, Ref),
      HS_n = HS - 1,
      set_size(H, HS_n),
      case HS_n =:= 0 of
        true -> ok;
        _ ->
          set_element(H, I, {Ref_n, Val}),
          set_map(H, Ref_n, I),
          combine(H, I, HS_n)
      end,
      ok
  end.

%% @doc Removes and returns the maximum priority element of a max heap.
-spec take_max(heap()) -> term() | {error, min_heap | empty_heap}.

take_max(H) when H#heap.mode =:= max -> pop(H);
take_max(_H) -> {error, min_heap}.

%% @doc Removes and returns the minimum priority element of a min heap.
-spec take_min(heap()) -> term() | {error, max_heap | empty_heap}.

take_min(H) when H#heap.mode =:= min -> pop(H);
take_min(_H) -> {error, max_heap}.

%% Deletes and returns the element at the top of the heap
%% and re-arranges the rest of the heap
-spec pop(heap()) -> term() | {error, empty_heap}.

pop(H) ->
  case get_element(H, 1) of
    [] -> {error, empty_heap};
    {RefHead, Head} ->
      HS = heap_size(H),
      %% delete/2
      {RefX, X} = get_element(H, HS),
      del_element(H, HS),
      del_map(H, RefHead),
      HS_n = HS - 1,
      set_size(H, HS_n),
      case HS_n =:= 0 of                                %% Can be commented
        true -> ok;                                     %% Can be commented
        false ->                                        %% Can be commented
          set_element(H, 1, {RefX, X}),
          set_map(H, RefX, 1),
          combine(H, 1, HS_n)
      end,                                              %% Can be commented
      Head
  end.

%% @doc Clear a heap
-spec clear(heap()) -> ok.

clear(H) ->
  Hs = get_size(H),
  [begin
     {Ref, _} = get_element(H, I),
     del_element(H, I),
     del_map(H, Ref)
   end || I <- lists:seq(1, Hs)],
  set_size(H, 0),
  ok.

%% @doc Clear a heap
-spec delete_heap(heap()) -> ok.

delete_heap(H) ->
  clear(H),
  del_capacity(H),
  del_size(H),
  ok.

%% @doc Returns the number of elements contained in a heap.
-spec heap_size(heap()) -> non_neg_integer().

heap_size(H) ->
  get_size(H).

%% @doc Returns the capacity of a heap.
-spec heap_capacity(heap()) -> non_neg_integer().

heap_capacity(H) ->
  get_capacity(H).

%% @doc Returns whether a heap is full or not.
-spec is_full(heap()) -> boolean().

is_full(H) ->
  Capacity = heap_capacity(H),
  Size = heap_size(H),
  is_integer(Capacity) andalso Size >= Capacity.

%% @doc Checks whether a heap is empty or not.
-spec is_empty(heap()) -> boolean().

is_empty(H) -> heap_size(H) =:= 0.

%% @doc Returns the element of a max heap with the maximum priority.
%% <p>If it is a min heap, it returns <code>{error, min_heap}</code>.</p>
-spec max(heap()) -> term() | {error, min_heap | empty_heap}.

max(H) when H#heap.mode =:= max ->
  case get_element(H, 1) of
    [] -> {error, empty_heap};
    {_Ref, Max} -> Max
  end;
max(_H) -> {error, min_heap}.

%% @doc Returns the element of a min heap with the minimum priority.
%% <p>If it is a max heap, it returns <code>{error, max_heap}</code>.</p>
-spec min(heap()) -> term() | {error, max_heap | empty_heap}.

min(H) when H#heap.mode =:= min ->
  case get_element(H, 1) of
    [] -> {error, empty_heap};
    {_Ref, Min} -> Min
  end;
min(_H) -> {error, max_heap}.

%% @doc Returns heap elements in list.
-spec to_list(heap()) -> kv_list().

to_list(H) ->
  C = heap_size(H),
  [get_element(H, I) || I <- lists:seq(1, C)].

%% @doc Returns all values of heap in list.
-spec get_values(heap()) -> [term()].

get_values(H) ->
  [Val || {_Ref, Val} <- to_list(H)].

%% @doc Returns whether is a valid heap.
-spec is_valid(heap()) -> boolean().

is_valid(H) ->
  C = heap_size(H),
  is_valid_loop(H, C).


%% =======================================================================
%% Internal Functions
%% =======================================================================

is_valid_loop(H, I) when I > 1 ->
  P = parent(I),
  #heap{mode = Mode, cmp = Cmp} = H,
  {_, ValI} = get_element(H, I),
  {_, ValP} = get_element(H, P),
  case {Mode, Cmp(ValI, ValP)} of
    {min, CR} when ?LT(CR) ->
      false;
    {max, CR} when ?GT(CR) ->
      false;
    _ ->
      is_valid_loop(H, I - 1)
  end;
is_valid_loop(_H, _I) ->
  true.

-spec up_heapify(heap(), pos_integer(), term(), reference()) -> ok.

up_heapify(H, I, X, Ref) ->
  set_element(H, I, {Ref, X}),
  set_map(H, Ref, I),
  P = parent(I),
  up_heapify2(H, I, P).

%% Re-arranges the heap in a bottom-up manner
-spec up_heapify2(heap(), pos_integer(), pos_integer()) -> ok.

up_heapify2(H, I, P) when I > 1 ->
  {_RefX, ValI} = get_element(H, I),
  {_RefY, ValP} = get_element(H, P),
  #heap{cmp = Cmp, mode = Mode} = H,
  case Cmp(ValI, ValP) of
    CR when Mode =:= max, ?GT(CR); Mode =:= min, ?LT(CR) ->
      swap(H, P, I),
      NI = P,
      NP = parent(NI),
      up_heapify2(H, NI, NP);
    _ -> ok
  end;
up_heapify2(_H, _I, _P) -> ok.

%% Used for constructing a heap from a list
-spec construct_heap(heap(), pos_integer(), pos_integer()) -> ok.

construct_heap(H, I, HS) when I > 0 ->
  combine(H, I, HS),
  construct_heap(H, I - 1, HS);
construct_heap(_H, _I, _HS) -> ok.

%% Re-arranges the heap in a top-down manner
-spec combine(heap(), pos_integer(), pos_integer()) -> ok.

combine(H, I, HS) ->
  L = left(I),
  R = right(I),
  MP = I,
  MP_L = combine_h1(H, L, MP, HS),
  MP_R = combine_h1(H, R, MP_L, HS),
  combine_h2(H, MP_R, I, HS).

-spec combine_h1(heap(), pos_integer(), pos_integer(), pos_integer()) -> pos_integer().

combine_h1(H, W, MP, HS) when W =< HS ->
  {_RefX, X} = get_element(H, W),
  {_RefY, Y} = get_element(H, MP),
  #heap{cmp = Cmp, mode = Mode} = H,
  case {Cmp(X, Y), Mode} of
    {CR, max} when ?GT(CR) -> W;
    {CR, min} when ?LT(CR) -> W;
    {_, _} -> MP
  end;
combine_h1(_H, _W, MP, _HS) -> MP.

-spec combine_h2(heap(), pos_integer(), pos_integer(), pos_integer()) -> ok.

combine_h2(_H, MP, I, _HS) when MP =:= I -> ok;
combine_h2(H, MP, I, HS) ->
  swap(H, I, MP),
  combine(H, MP, HS).

-spec down_heapify(heap(), pos_integer(), term(), reference()) -> ok.

down_heapify(H, I, X, Ref) ->
  set_element(H, I, {Ref, X}),
  HS = heap_size(H),
  combine(H, I, HS).

%% Swaps two elements of the heap
-spec swap(heap(), pos_integer(), pos_integer()) -> true.

swap(H, I, J) ->
  {RX, X} = get_element(H, I),
  {RY, Y} = get_element(H, J),
  set_element(H, I, {RY, Y}),
  set_map(H, RY, I),
  set_element(H, J, {RX, X}),
  set_map(H, RX, J).

-spec ets_from_elements(heap(), kv_list()) -> integer().

ets_from_elements(H, L) ->
  HS = add_elements(H, L, 1),
  set_size(H, HS),
  HS.

-spec add_elements(heap(), kv_list(), pos_integer()) -> integer().

add_elements(_H, [], N) ->
  N - 1;
add_elements(H, [{Key, Val} | Ts], N) ->
  set_element(H, N, {Key, Val}),
  set_map(H, Key, N),
  add_elements(H, Ts, N + 1);
add_elements(H, [_ | Ts], N) ->
  add_elements(H, Ts, N).

%% @doc default comparetor
-spec cmp(term(), term()) -> integer().

cmp(L, R) ->
  if
    L > R -> 1;
    L < R -> -1;
    true -> 0
  end.

wrap_key(Key) ->
  {?MODULE, Key}.

%% @doc Returns parent index, i div 2
-spec parent(integer()) -> integer().

parent(Index) ->
  Index bsr 1.

%% @doc Returns left child index, 2 * i
-spec left(integer()) -> integer().

left(Index) ->
  Index bsl 1.

%% @doc Returns right child index, 2 * i + 1
-spec right(integer()) -> integer().

right(Index) ->
  Index bsl 1 + 1.


%% =======================================================================
%% Data Storage
%% =======================================================================

-spec get_element(heap(), integer()) -> tuple() | [].
get_element(H, I) ->
  case ets:lookup(H#heap.htab, ?KEY({H#heap.id, element, I})) of
    [{_, V}] -> V;
    _ -> []
  end.

-spec set_element(heap(), integer(), term()) -> true.
set_element(H, I, V) ->
  ets:insert(H#heap.htab, {?KEY({H#heap.id, element, I}), V}).

-spec del_element(heap(), integer()) -> true.
del_element(H, I) ->
  ets:delete(H#heap.htab, ?KEY({H#heap.id, element, I})).

-spec get_map(heap(), term()) -> integer() | [].
get_map(H, K) ->
  case ets:lookup(H#heap.htab, ?KEY({H#heap.id, key_index_map, K})) of
    [{_, V}] -> V;
    _ -> []
  end.

-spec set_map(heap(), term(), integer()) -> true.
set_map(H, K, I) ->
  ets:insert(H#heap.htab, {?KEY({H#heap.id, key_index_map, K}), I}).

-spec del_map(heap(), term()) -> true.
del_map(H, K) ->
  ets:delete(H#heap.htab, ?KEY({H#heap.id, key_index_map, K})).

-spec get_capacity(heap()) -> capacity().
get_capacity(H) ->
  case ets:lookup(H#heap.htab, ?KEY({H#heap.id, capacity})) of
    [{_, C}] -> C;
    _ -> 0
  end.

-spec set_capacity(heap(), capacity()) -> true.
set_capacity(H, C) ->
  ets:insert(H#heap.htab, {?KEY({H#heap.id, capacity}), C}).

-spec del_capacity(heap()) -> true.
del_capacity(H) ->
  ets:delete(H#heap.htab, ?KEY({H#heap.id, capacity})).

-spec get_size(heap()) -> integer().
get_size(H) ->
  case ets:lookup(H#heap.htab, ?KEY({H#heap.id, size})) of
    [{_, S}] -> S;
    _ -> 0
  end.

-spec set_size(heap(), integer()) -> true.
set_size(H, S) ->
  ets:insert(H#heap.htab, {?KEY({H#heap.id, size}), S}).

-spec del_size(heap()) -> true.
del_size(H) ->
  ets:delete(H#heap.htab, ?KEY({H#heap.id, size})).
