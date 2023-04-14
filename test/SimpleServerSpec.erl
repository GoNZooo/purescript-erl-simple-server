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
  fun() -> check_hibernate_with_timeout(Pid, 250) end.

get_hibernation_status(Pid) ->
  case process_info(Pid, current_function) of
    {current_function, {erlang, hibernate, 3}} ->
      true;
    _ ->
      false
  end.

check_hibernate_with_timeout(_Pid, Timeout) when Timeout =< 0 ->
  false;
check_hibernate_with_timeout(Pid, Timeout) ->
  WaitTime = 25,
  case get_hibernation_status(Pid) of
    true ->
      true;
    false ->
      receive
        _Other -> false
      after WaitTime ->
        check_hibernate_with_timeout(Pid, Timeout - WaitTime)
      end
  end.
