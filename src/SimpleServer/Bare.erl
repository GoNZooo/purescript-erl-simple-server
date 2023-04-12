-module(simpleServer_bare@foreign).

-export([startLink_/2, cast/2, call/2, serverLoop/5]).

% `name` is either `{nothing}` or `{just, Name}` where `Name` is
% `{local, Name}`, `{global, Name}`, or `{via, Module, Name}`.
startLink_(StartArguments,
           #{init := Init,
             handleInfo := HandleInfo,
             handleContinue := HandleContinue,
             terminate := Terminate,
             name := {nothing}}) ->
  fun() ->
     Pid =
       spawn_link(?MODULE,
                  serverLoop,
                  [StartArguments, Init, HandleInfo, HandleContinue, Terminate]),
     {right, Pid}
  end;
startLink_(StartArguments,
           #{init := Init,
             handleInfo := HandleInfo,
             handleContinue := HandleContinue,
             terminate := Terminate,
             name := {just, Name}}) ->
  fun() ->
     MaybePid = get_name(Name),
     case MaybePid of
       Pid when is_pid(Pid) -> {left, {alreadyStarted, Pid}};
       undefined ->
         Pid =
           spawn_link(?MODULE,
                      serverLoop,
                      [StartArguments, Init, HandleInfo, HandleContinue, Terminate]),
         RegistrationResult = try_register(Name, Pid),
         translate_registration_result(RegistrationResult, Pid)
     end
  end.

serverLoop(StartArguments, Init, HandleInfo, HandleContinue, Terminate) ->
  case (Init(StartArguments))() of
    {simpleInitOk, State} ->
      loop(State, HandleInfo, HandleContinue, Terminate);
    {simpleInitContinue, State, Continue} ->
      handle_continue(Continue, State, HandleInfo, HandleContinue, Terminate);
    {simpleInitError, Foreign} ->
      exit({simpleInitError, Foreign})
  end.

loop(State, HandleInfo, HandleContinue, Terminate) ->
  receive
    {cast, F} ->
      case (F(State))() of
        {simpleNoReply, NewState} ->
          loop(NewState, HandleInfo, HandleContinue, Terminate);
        {simpleContinue, Continue, NewState} ->
          handle_continue(Continue, NewState, HandleInfo, HandleContinue, Terminate);
        {simpleReply, _Reply, _NewState} ->
          throw({reply_not_allowed, {cast, F}});
        {simpleStop, Reason, NewState} ->
          terminate(Reason, NewState, Terminate)
      end;
    {call, F, From, Ref} ->
      case ((F(From))(State))() of
        {simpleCallReply, Reply, NewState} ->
          From ! {simpleReply, Ref, Reply},
          loop(NewState, HandleInfo, HandleContinue, Terminate);
        {simpleCallReplyContinue, Reply, NewState, Continue} ->
          From ! {simpleReply, Ref, Reply},
          handle_continue(Continue, NewState, HandleInfo, HandleContinue, Terminate);
        {simpleCallStop, Reason, NewState} ->
          terminate(Reason, NewState, Terminate)
      end;
    Message ->
      case ((HandleInfo(Message))(State))() of
        {simpleNoReply, NewState} ->
          loop(NewState, HandleInfo, HandleContinue, Terminate);
        {simpleContinue, Continue, NewState} ->
          handle_continue(Continue, NewState, HandleInfo, HandleContinue, Terminate);
        {simpleStop, Reason, NewState} ->
          terminate(Reason, NewState, Terminate)
      end
  end.

handle_continue(Continue, State, HandleInfo, HandleContinue, Terminate) ->
  case ((HandleContinue(Continue))(State))() of
    {simpleNoReply, NewState} ->
      loop(NewState, HandleInfo, HandleContinue, Terminate);
    {simpleContinue, NewContinue, NewState} ->
      handle_continue(NewContinue, NewState, HandleInfo, HandleContinue, Terminate);
    {simpleStop, Reason, NewState} ->
      terminate(Reason, NewState, Terminate)
  end.

terminate(Reason, State, Terminate) ->
  ((Terminate(Reason))(State))(),
  exit(self(), translate_stop_reason(Reason)).

cast(PidOrNameReference, F) ->
  fun() -> send_message_to_name(PidOrNameReference, {cast, F}) end.

call(Pid, F) ->
  fun() ->
     Ref = make_ref(),
     From = self(),
     send_message_to_name(Pid, {call, F, From, Ref}),
     receive {simpleReply, Ref, Reply} -> Reply end
  end.

send_message_to_name(PidOrNameReference, Message) ->
  Name = translate_process_reference(PidOrNameReference),
  case get_name(Name) of
    Pid when is_pid(Pid) ->
      Pid ! Message;
    undefined ->
      throw({unable_to_find_process_with_name, Name})
  end.

get_name({local, Name}) ->
  whereis(Name);
get_name({global, Name}) ->
  global:whereis_name(Name);
get_name({via, Module, Name}) ->
  Module:whereis(Name);
get_name(Name) when is_atom(Name) ->
  whereis(Name).

try_register({local, Name}, Pid) ->
  register(Name, Pid);
try_register({global, Name}, Pid) ->
  global:register_name(Name, Pid);
try_register({via, Module, Name}, Pid) ->
  Module:register(Name, Pid).

translate_registration_result({ok, Pid}, Pid) ->
  {right, Pid};
translate_registration_result({error, {already_registered, Pid}}, Pid) ->
  {left, {alreadyStarted, Pid}};
translate_registration_result(true, Pid) ->
  {right, Pid};
translate_registration_result(yes, Pid) ->
  {right, Pid};
translate_registration_result(false, _Pid) ->
  {left, {failed, unable_to_register}};
translate_registration_result(no, _Pid) ->
  {left, {failed, unable_to_register}}.

translate_process_reference({pidReference, Pid}) ->
  Pid;
translate_process_reference({nameReference, {local, Name}}) ->
  Name;
translate_process_reference({nameReference, Name}) ->
  Name.

translate_stop_reason({stopNormal}) ->
  normal;
translate_stop_reason({stopShutdown, {nothing}}) ->
  shutdown;
translate_stop_reason({stopShutdown, {just, Reason}}) ->
  {shutdown, Reason};
translate_stop_reason({stopOther, Reason}) ->
  Reason.
