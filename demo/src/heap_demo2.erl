-module(heap_demo2).

-export([heap_test/0]).

heap_test() ->
  E = ets:new(minheap, [public, named_table]),
  min_heap(E),
  max_heap(E),
  ok.

min_heap(E) ->
  H1 = heap2:new(min_heap1, E, min, infinity),
  0 = heap2:heap_size(H1),
  true = heap2:is_empty(H1),

  H2 = heap2:from_list(min_heap2, E, min, 5, [{1, 5}, {2, 4}, {3, 3}, {4, 7}, {5, 10}]),
  5 = heap2:heap_size(H2),
  true = heap2:is_valid(H2),

  %% update an element
  heap2:update(H2, 4, 1),
  1 = heap2:min(H2),
  true = heap2:is_valid(H2),

  %% insert an element
  true = heap2:is_full(H2),
  {error, full} = heap2:insert(H2, 6, 13),
  true = heap2:is_valid(H2),

  %% pop root element
  1 = heap2:min(H2),
  1 = heap2:take_min(H2),
  4 = heap2:heap_size(H2),
  true = heap2:is_valid(H2),

  %% insert or update an element
  ok = heap2:insert_or_update(H2, 6, 0),
  0 = heap2:min(H2),
  true = heap2:is_full(H2),
  true = heap2:is_valid(H2),

  %% delete an element
  heap2:delete(H2, 6),
  3 = heap2:min(H2),
  4 = heap2:heap_size(H2),
  true = heap2:is_valid(H2),

  %% user defined comparetor

  ok.

max_heap(E) ->
  H1 = heap2:new(max_heap1, E, max, infinity),
  0 = heap2:heap_size(H1),
  true = heap2:is_empty(H1),

  H2 = heap2:from_list(max_heap2, E, max, 5, [{1, 15}, {2, 14}, {3, 13}, {4, 17}, {5, 20}]),
  5 = heap2:heap_size(H2),
  true = heap2:is_valid(H2),

  %% update an element
  heap2:update(H2, 4, 21),
  21 = heap2:max(H2),
  true = heap2:is_valid(H2),

  %% insert an element
  true = heap2:is_full(H2),
  {error, full} = heap2:insert(H2, 6, 13),
  true = heap2:is_valid(H2),

  %% pop root element
  21 = heap2:max(H2),
  21 = heap2:take_max(H2),
  4 = heap2:heap_size(H2),
  true = heap2:is_valid(H2),

  %% insert or update an element
  ok = heap2:insert_or_update(H2, 6, 24),
  24 = heap2:max(H2),
  true = heap2:is_full(H2),
  true = heap2:is_valid(H2),

  %% delete an element
  heap2:delete(H2, 6),
  20 = heap2:max(H2),
  4 = heap2:heap_size(H2),
  true = heap2:is_valid(H2),

  %% user defined comparetor

  ok.

