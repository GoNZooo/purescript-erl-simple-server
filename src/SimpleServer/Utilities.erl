-module(simpleServer_utilities@foreign).

-export([selfPid/0, sendSelf/1]).

selfPid() ->
  fun() -> self() end.

sendSelf(Message) ->
  fun() -> self() ! Message end.

