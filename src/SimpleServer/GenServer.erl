-module(simpleServer_genServer@foreign).

-export([startLink_/2, cast/2, call/2, init/1, handle_info/2, handle_cast/2,
         handle_continue/2, handle_call/3, terminate/2]).

-record(state,
        {state :: term(),
         handleInfo :: handle_info_function(),
         handleContinue :: handle_continue_function(),
         terminate :: terminate_function(),
         specified_stop_reason = undefined :: term() | undefined}).

-type handle_info_function() :: fun((term(), term()) -> handle_info_return()).
-type handle_info_return() ::
  {simpleNoReply, term()} | {simpleStop, term(), term()} | {simpleContinue, term(), term()}.
-type handle_continue_function() :: fun((term(), term()) -> handle_info_return()).
-type terminate_function() :: fun((term(), term()) -> term()).

startLink_(StartArguments,
           #{init := Init,
             handleInfo := HandleInfo,
             handleContinue := HandleContinue,
             terminate := Terminate,
             name := {nothing}}) ->
  fun() ->
     case gen_server:start_link(?MODULE,
                                {StartArguments, Init, HandleInfo, HandleContinue, Terminate},
                                [])
     of
       {ok, Pid} -> {right, Pid};
       ignore -> {left, ignore};
       {error, {already_started, Pid}} -> {left, {alreadyStarted, Pid}};
       {error, Reason} -> {left, {failed, Reason}}
     end
  end;
startLink_(StartArguments,
           #{init := Init,
             handleInfo := HandleInfo,
             handleContinue := HandleContinue,
             terminate := Terminate,
             name := {just, Name}}) ->
  fun() ->
     case gen_server:start_link(Name,
                                ?MODULE,
                                {StartArguments, Init, HandleInfo, HandleContinue, Terminate},
                                [])
     of
       {ok, Pid} -> {right, Pid};
       ignore -> {left, ignore};
       {error, {already_started, Pid}} -> {left, {alreadyStarted, Pid}};
       {error, Reason} -> {left, {failed, Reason}}
     end
  end.

cast(PidOrNameReference, F) ->
  Name = translate_process_reference(PidOrNameReference),
  fun() -> gen_server:cast(Name, {cast, F}) end.

call(PidOrNameReference, F) ->
  Name = translate_process_reference(PidOrNameReference),
  fun() -> gen_server:call(Name, {call, F}) end.

init({StartArguments, Init, HandleInfo, HandleContinue, Terminate}) ->
  case (Init(StartArguments))() of
    {simpleInitOk, State} ->
      {ok,
       #state{state = State,
              handleInfo = HandleInfo,
              handleContinue = HandleContinue,
              terminate = Terminate}};
    {simpleInitContinue, State, Continue} ->
      {ok,
       #state{state = State,
              handleInfo = HandleInfo,
              handleContinue = HandleContinue,
              terminate = Terminate},
       {continue, Continue}};
    {simpleInitError, Foreign} ->
      {stop, Foreign};
    {simpleInitStop, Reason, State} ->
      {stop,
       translate_stop_reason(Reason),
       #state{specified_stop_reason = Reason, state = State}};
    {simpleInitHibernate, State} ->
      {ok,
       #state{state = State,
              handleInfo = HandleInfo,
              handleContinue = HandleContinue,
              terminate = Terminate},
       hibernate}
  end.

handle_cast({cast, F}, #state{state = State} = ServerState) ->
  case (F(State))() of
    {simpleNoReply, NewState} ->
      {noreply, ServerState#state{state = NewState}};
    {simpleContinue, Continue, NewState} ->
      {noreply, ServerState#state{state = NewState}, {continue, Continue}};
    {simpleStop, Reason, NewState} ->
      {stop,
       translate_stop_reason(Reason),
       ServerState#state{state = NewState, specified_stop_reason = Reason}};
    {simpleHibernate, NewState} ->
      {noreply, ServerState#state{state = NewState}, hibernate}
  end.

handle_call({call, F}, From, #state{state = State} = ServerState) ->
  case ((F(From))(State))() of
    {simpleCallReply, Reply, NewState} ->
      {reply, Reply, ServerState#state{state = NewState}};
    {simpleCallReplyContinue, Reply, NewState, Continue} ->
      {reply, Reply, ServerState#state{state = NewState}, {continue, Continue}};
    {simpleCallStop, Reply, Reason, NewState} ->
      {stop,
       translate_stop_reason(Reason),
       Reply,
       ServerState#state{state = NewState, specified_stop_reason = Reason}};
    {simpleCallHibernate, Reply, NewState} ->
      {reply, Reply, ServerState#state{state = NewState}, hibernate}
  end.

handle_info(Message, #state{state = State, handleInfo = HandleInfo} = ServerState) ->
  case ((HandleInfo(Message))(State))() of
    {simpleNoReply, NewState} ->
      {noreply, ServerState#state{state = NewState}};
    {simpleContinue, Continue, NewState} ->
      {noreply, ServerState#state{state = NewState}, {continue, Continue}};
    {simpleStop, Reason, NewState} ->
      {stop,
       translate_stop_reason(Reason),
       ServerState#state{state = NewState, specified_stop_reason = Reason}}
  end.

handle_continue(Continue,
                #state{state = State, handleContinue = HandleContinue} = ServerState) ->
  case ((HandleContinue(Continue))(State))() of
    {simpleNoReply, NewState} ->
      {noreply, ServerState#state{state = NewState}};
    {simpleContinue, NewContinue, NewState} ->
      {noreply, ServerState#state{state = NewState}, {continue, NewContinue}};
    {simpleStop, Reason, NewState} ->
      {stop,
       translate_stop_reason(Reason),
       ServerState#state{state = NewState, specified_stop_reason = Reason}};
    {simpleHibernate, NewState} ->
      {noreply, ServerState#state{state = NewState}, hibernate}
  end.

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

terminate(_Reason,
          #state{state = State,
                 terminate = Terminate,
                 specified_stop_reason = StopReason}) ->
  ((Terminate(StopReason))(State))().
