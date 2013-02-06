%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Main application supervisor.
%% @private
-module(sl_sup).
-created('Date: 25/12/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([spawn_worker/3, kill_worker/1, call_worker/2]).

%% ---------------------------------------------------------------------
%% Public methods.
%% ---------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 5, 60}, [
        {sl,
            {sl_worker, start_link, []},
            permanent,
            5000,
            worker,
            [sl_worker]}
    ]}}.

spawn_worker(App, Log, Opts) ->
    supervisor:start_child(?MODULE, [App, Log, Opts]).

kill_worker(App) ->
    supervisor:terminate_child(?MODULE,
        whereis(sl_worker:get_worker_name(App))).

call_worker(App, Msg) ->
    gen_server:call(sl_worker:get_worker_name(App), Msg).
