-module(test_simpleServerSpec@foreign).

-export([receiveAtom/0, selfPid/0, isHibernating/1]).

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

isHibernating(Pid) ->
  fun() ->
     case process_info(Pid, current_function) of
       {current_function, {erlang, hibernate, 3}} -> true;
       _ -> false
     end
  end.
