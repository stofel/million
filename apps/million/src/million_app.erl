%%%-------------------------------------------------------------------
%% @doc million public API
%% @end
%%%-------------------------------------------------------------------

-module(million_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    start_process_cluster(),
    million_sup:start_link().

stop(_State) ->
    ok.

%% Стартуем кластер воркеров
%% Имя кастера million_cluster
%% Количетсво нод 1 локальная
%% Размер пула супервизоворов обслуживающих воркеры 60
%%    (наверно многовато, по идее достаточно 2-3 воркера на ядро)
start_process_cluster() ->
  DbPath = "./var/db/ecldb",
  CName = million_cluster,
  Args = #{
    new  => do_try, %% true|false|do_try Start or not new cluster. 
                    %% if do_try, look cluster file existance, if not exists start new cluster
    ma   => {million_worker, #{}}, %% Module Arguments to start worker process
    opt  => empty_opts,
    path => DbPath}, %% Path for dinamic compile files with rings and monitoring

  Nodes = [node()],
  %% med = million ecldb domain
  Domes = [list_to_atom("med"++ integer_to_list(I)) || I <- lists:seq(1,60)],

  %% Start Cluster
  ClusterStopTimeout = 150000, %% msec (Время ожидания завершения всех процессов)
  [{ok, _} = rpc:call(N, ecldb, start_cluster, [CName, Args, ClusterStopTimeout]) || N <- Nodes],
  %% Add Nodes
  [ok      = ecldb:add_node(CName, N) || N <- Nodes],
  %% Add Domains
  [{ok, _} = rpc:call(N, ecldb, add_domain, [CName, D]) || D <- Domes, N <- Nodes],
  %% Apply changes
  ok = ecldb:merge(CName),
  ok = ecldb:norma(CName),
  ok.

