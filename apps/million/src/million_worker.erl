%%
%% Exldb cluster worker.
%% Process with 120sec life time
%% after make million:test() you can make some debug (in ~100 sec period)
%% like ping processes, stats etc
%% ecldb:list(), ecldb:stat(million_cluster),
%% million:hello(<<"123">>, <<"john">>), observer_cli:start()
%% and any time measurement
%%

-module(million_worker).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% Api
-export([
  call/2,
  call/3,
  info/2
]).

-define(CLUSTER_NAME, million_cluster).
-define(WORKER_TTL, 120*1000). %% Process with 120sec life time


%% Синхронная передача команды в запущеный или незапущенный воркер
-spec call(Key::binary(), Msg::map()) -> Reply::term().
call(Key, Msg) ->
    call(Key, Msg, #{}).
call(Key, Msg, Opts) ->
    Flags = Opts#{mode => start}, %% Start worker if not exists
    case ecldb:call(?CLUSTER_NAME, Key, Msg, Flags) of
        unreg -> %% Case worker in unreg (shutdown) state to omit race condition 
            ecldb:call(?CLUSTER_NAME, Key, Msg, Flags);
        Reply ->
            Reply
    end.


%% Синхронная передача комaнды в запущеный воркер
-spec info(Key::binary(), Msg::map()) -> not_started|Reply::term().
info(Key, Msg) ->
    Flags = #{mode => info},
    case ecldb:call(?CLUSTER_NAME, Key, Msg, Flags) of
        unreg ->
            ecldb:call(?CLUSTER_NAME, Key, Msg, Flags);
        not_started ->
            %% log it
            not_started;
        Reply ->
            Reply
    end.


%% Init worker process
init(_Args = #{reg := Reg = #{key := WorkerNameKey, from := From},
               opts := _Opts}) ->
    NowMs = os:system_time(millisecond),
    case NowMs > 0 of
        true ->
            State = #{
                worker_name => WorkerNameKey,
                start_time => NowMs,
                last_cast_time => no_one_cast,
                last_call_time => no_one_call
            },
            ecldb_domain:reg(Reg#{reply => ok}),
            {ok, State#{reg => Reg#{pid => self()}}, ?WORKER_TTL};
        false -> %% Example of init error
            %% debug log 
            ErrReply = {err, {wrong_system_time, <<"tratatata mod:line">>}},
            gen_server:reply(From, ErrReply),
            {stop, normal}
      end.


%% Самостоятельное завершение, по таймауту
terminate(_Reason, unreg) ->
    ok;
%% Принудительное завершение, остановка кластера или сервиса
terminate(Reason, S) ->
    _ = exit_signal(Reason, S),
    ok.


%%
exit_signal(Reason, S) when Reason == normal; Reason == shutdown ->
    %% save state to db for example due to process signal EXIT exit
    {stop, shutdown, S};
exit_signal(Reason, S) ->
    %% debug log
    {stop, Reason, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api {{{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% infos
handle_info(timeout, State)                 -> timeout_(State);
handle_info(_Msg, unreg)                    -> {noreply, unreg, 1000};  %% debug log
handle_info({'EXIT', _Pid, Reason}, S)      -> exit_signal(Reason, S);
handle_info(_Msg, S)                        -> {noreply, S, ?WORKER_TTL}. %% debug log
%% casts
handle_cast(_Msg, unreg)                    -> {noreply, unreg, 1000};
handle_cast({million_worker_msg, Msg}, S)   -> cast_receive_msg(S, Msg);
handle_cast(_Msg, S)                        -> {noreply, S, ?WORKER_TTL}. %% debug log
%% calls
handle_call(_Msg, _F, unreg)                -> {reply, unreg, unreg, 1000};
handle_call({million_worker_msg, Msg},_F,S) -> call_receive_msg(S, Msg);
handle_call(_Msg, _F, S)                    -> 
    {reply, {err, {unk_cmd, <<"gagagag mod:line">>}}, S, ?WORKER_TTL}. %% debug log
%% }}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
timeout_(_S = #{reg := Reg}) ->
    %% save State to database for example due to process timeout exit
    try ecldb_domain:unreg(Reg) catch _:_:_ -> do_nothing end,
    {noreply, unreg, 1000};
timeout_(unreg) ->
    {stop, normal, unreg}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MSG menegment (For example some unnecessary logic)
cast_receive_msg(S, _Msg) ->
    NewS = S#{last_cast_time := os:system_time(millisecond)},
    {noreply, NewS, ?WORKER_TTL}.


call_receive_msg(S0, Msg) ->
    S = S0#{last_cast_time := os:system_time(millisecond)},
    {WorkerReply, NewS} =
        try case Msg of
            ping            -> ping(S);
            {hello, Name}   -> hello(S, Name);
            _ElseMsg        -> {err, {unknown_msg, <<"tratata mod:line">>}} %% debug log
        end of
            {ok, Reply, NewStateValue} ->
                {Reply, NewStateValue};
            {err, _} = Err ->
                {Err, S}
        catch
            _Err:_Code:_Trace ->
                %% log it
                {{err, {crash, <<"papaparam mod:line:log_record_id">>}}, S0}
        end,
    {reply, WorkerReply, NewS, ?WORKER_TTL}.


ping(S) ->
    {ok, pong, S}.


hello(S = #{worker_name := WorkerName}, Name) ->
    {ok, {hello, Name, WorkerName}, S}.
