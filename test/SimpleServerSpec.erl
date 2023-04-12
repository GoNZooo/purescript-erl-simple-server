-module(test_simpleServerSpec@foreign).

-export([receiveAtom/0, selfPid/0]).

receiveAtom() ->
  fun() ->
     receive
       Atom when is_atom(Atom) -> {just, Atom};
       _Other -> {nothing}
     after 1000 -> throw(timeout_waiting_for_atom)
     end
  end.

selfPid() ->
  fun() -> self() end.
