%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Worker than handle log requests.
%% @private
-module(sl_worker).
-created('Date: 25/12/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-behaviour(gen_server).

-export([
    start_link/3,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    get_worker_name/1,
    get_opts/1
]).

%% ---------------------------------------------------------------------
%% Public methods.
%% ---------------------------------------------------------------------
%% @hidden
start_link(App, LogFile, Opts) ->
    Name = get_worker_name(App),
    gen_server:start_link({local, Name}, ?MODULE, [App, LogFile, Opts], []).

-spec get_worker_name(atom()) -> atom().
get_worker_name(App) ->
    list_to_atom("sl_worker_" ++ atom_to_list(App)).

-spec get_opts(atom()) -> list(atom() | tuple()).
get_opts(App) ->
    case application:get_env(App, sl_opts) of
        {ok, Opts} -> Opts;
        _ -> []
    end.

%% ---------------------------------------------------------------------
%% gen_server specific.
%% ---------------------------------------------------------------------
%% @hidden
init([App, LogFile, Opts]) ->
    process_flag(trap_exit, true),
    [{_, Pid}] = ets:lookup(ac_tab, {application_master, App}),
    link(Pid),
    application:set_env(App, sl_opts, Opts),
    {ok, LogId} = disk_log:open([
        {name, get_worker_name(App)},
        {file, LogFile},
        {format, external}
    ]),
    {ok, {LogId, App}}.

handle_call({log, {_Severity, Item}}, _From, State = {LogId, _App}) ->
    {reply, disk_log:balog(LogId, Item), State};
handle_call(_Message, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Message, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, {LogId, App}) ->
    application:unset_env(App, sl_opts),
    disk_log:close(LogId).

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
