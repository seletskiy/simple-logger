%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc General API and worker that handle log requests.
-module(sl).
-created('Date: 25/12/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([open/1, open/2, close/0, log/2, log/3]).
-export([debug/1, debug/2, info/1, info/2, warn/1, warn/2, error/1, error/2,
    critical/1, critical/2]).
-export([format_date/0]).

-record(log, {
    format :: fun(),
    name :: atom(),
    file :: list()
}).

-record(state, {
    logs  = [] :: list(#log{})
}).

%% ---------------------------------------------------------------------
%% Public methods.
%% ---------------------------------------------------------------------
%% @hidden
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Open specified log file for use and make it available
%%      for current application.
%%
%%      Currently only one log per application and one top level
%%      log is supported.
%%
%%      Top level log is a log that opened without application context.
%%
%%      Application name obtained via `application:get_application/0' call.
%%
%%      If there are no log opened for current application, top level log
%%      will be used, if it was opened.
-spec open(list()) -> ok | {error, already_open}.
open(LogFile) ->
    open(LogFile, []).

%% @doc Same as {@link open/1}, but with additional opts.
%%      `Opts' is a proplist with keys:
%%      <ul>
%%      <li>`format' — specifies `fun/3' that should format incoming item before
%%        writing to log
%%
%%        `fun/3' must have 3 arguments: `Severity', `ItemToLog', `CallerInfo'.
%%
%%        `fun/3' should return `list()' result that represents formatted item.
%%
%%        Default format is: `[DD/MM/YY HH:MM:SS.MIC] Pid Module (Severity) ...'
%%        </li>
%%      </ul>
-spec open(list(), list()) -> ok | {error, already_open}.
open(LogFile, Opts) ->
    gen_server:call(?MODULE, {open, LogFile, Opts, get_caller()}).

%% @doc Close log and flush it to disk.
-spec close() -> ok | {error, log_is_not_open}.
close() ->
    gen_server:call(?MODULE, close).

%% @doc Log item with `debug' severity. See {@link log/2} for more info.
-spec debug(list()) -> ok | {error, log_is_not_open}.
debug(Item) ->
    log(Item, "debug").

%% @doc Same as {@link debug/1}, but uses io_lib:format to format `Item'
%%      with `Args'. See {@link log/2} for more info.
-spec debug(list(), list()) -> ok | {error, log_is_not_open}.
debug(Item, Args) ->
    log(Item, "debug", Args).

%% @doc Log item with `info' severity. See {@link log/2} for more info.
-spec info(list()) -> ok | {error, log_is_not_open}.
info(Item) ->
    log(Item, "info").

%% @doc Same as {@link info/1}, but uses io_lib:format to format `Item'
%%      with `Args'. See {@link log/2} for more info.
-spec info(list(), list()) -> ok | {error, log_is_not_open}.
info(Item, Args) ->
    log(Item, "info", Args).

%% @doc Log item with `warn' severity. See {@link log/2} for more info.
-spec warn(list()) -> ok | {error, log_is_not_open}.
warn(Item) ->
    log(Item, "warning").

%% @doc Same as {@link warn/1}, but uses io_lib:format to format `Item'
%%      with `Args'. See {@link log/2} for more info.
-spec warn(list(), list()) -> ok | {error, log_is_not_open}.
warn(Item, Args) ->
    log(Item, "warning", Args).

%% @doc Log item with `error' severity. See {@link log/2} for more info.
-spec error(list()) -> ok | {error, log_is_not_open}.
error(Item) ->
    log(Item, "error").

%% @doc Same as {@link error/1}, but uses io_lib:format to format `Item'
%%      with `Args'. See {@link log/2} for more info.
-spec error(list(), list()) -> ok | {error, log_is_not_open}.
error(Item, Args) ->
    log(Item, "error", Args).

%% @doc Log item with `critical' severity. See {@link log/2} for more info.
-spec critical(list()) -> ok | {error, log_is_not_open}.
critical(Item) ->
    log(Item, "critical").

%% @doc Same as {@link critical/1}, but uses io_lib:format to format `Item'
%%      with `Args'. See {@link log/2} for more info.
-spec critical(list(), list()) -> ok | {error, log_is_not_open}.
critical(Item, Args) ->
    log(Item, "critical", Args).

%% @doc Common method for logging with custom `Severity'.
%%      This call will automatically determines what log file it should use
%%      in current application (obtained via `application:get_application/0').
%%
%%      If there are no application context, then top level log will be used,
%%      if any.
-spec log(list(), list()) -> ok | {error, log_is_not_open}.
log(Item, Severity) ->
    gen_server:call(?MODULE, {log, Severity, Item, get_caller()}).

%% @doc Same as {@link log/2}, but uses io_lib:format to format `Item'
%%      with `Args'.
-spec log(list(), list(), list()) -> ok | {error, log_is_not_open}.
log(Item, Severity, Args) when is_list(Args) ->
    log(io_lib:format(Item, Args), Severity).

%% ---------------------------------------------------------------------
%% Misc.
%% ---------------------------------------------------------------------
format_date() ->
    {Year, Month, Day} = date(),
    {Hour, Minute, Second} = time(),
    {_, _, Microsec} = now(),
    io_lib:format("~2..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B.~3..0B", [
        Day, Month, Year rem 1000,
        Hour, Minute, Second,
        Microsec div 1000]).

%% ---------------------------------------------------------------------
%% Private methods.
%% ---------------------------------------------------------------------
format_default(Severity, Item, {_CallerApp, CallerPid, CallerMod}) ->
    io_lib:format("[~s] ~11s ~-10s (~8ts) ~ts~n", [
        format_date(),
        pid_to_list(CallerPid), CallerMod,
        string:to_upper(Severity), Item
    ]).

create_log(LogFile, Opts, App) ->
    #log{
        name   = App,
        file   = LogFile,
        format = proplists:get_value(format, Opts, fun format_default/3)
    }.

open_log(LogFile, Opts, Caller = {App, _, _}) ->
    Log = create_log(LogFile, Opts, App),
    {ok, _} = disk_log:open([
        {name, Log#log.name},
        {file, LogFile},
        {format, external}
    ]),
    ok = log_item(Log, "info",
        io_lib:format("log started: ~w", [Log#log.name]), Caller),
    {ok, Log}.

close_log(Log, Caller) ->
    ok = log_item(Log, "info",
        io_lib:format("log shutdown: ~w", [Log#log.name]), Caller),
    ok = disk_log:close(Log#log.name),
    ok.

close_all_logs([]) ->
    ok;
close_all_logs([{_, Log} | Tail]) ->
    close_log(Log, get_caller()),
    close_all_logs(Tail).

log_item(Log, Severity, Item, Caller) ->
    Format = Log#log.format,
    disk_log:balog(Log#log.name, Format(Severity, Item, Caller)).

get_caller() ->
    CallerApp = case application:get_application() of
        {ok, Name} ->
            Name;
        _ ->
            default
    end,
    CallerPid = self(),
    CallerMod = element(1, lists:nth(3, element(2, element(2,
        catch erlang:error([]))))),
    {CallerApp, CallerPid, CallerMod}.

%% ---------------------------------------------------------------------
%% gen_server specific.
%% ---------------------------------------------------------------------
%% @hidden
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%% @hidden
handle_call({open, LogFile, Opts, Caller = {App, _, _}}, _From, State) ->
    case proplists:get_value(App, State#state.logs) of
        undefined ->
            {ok, Log} = open_log(LogFile, Opts, Caller),
            {reply, ok,
                State#state{logs =
                    State#state.logs ++ [{App, Log}]}};
        _ ->
            {reply, {error, log_already_open}, State}
    end;
handle_call({close, Caller = {App, _, _}}, _From, State) ->
    case proplists:get_value(App, State#state.logs) of
        undefined ->
            {reply, log_is_not_open, State};
        Log ->
            ok = close_log(Log, Caller),
            {reply, ok,
                State#state{logs =
                    lists:keydelete(App, 1, State#state.logs)}}
    end;
handle_call({log, Severity, Item, Caller = {App, _, _}}, _From, State) ->
    CommonLog = proplists:get_value(undefined, State#state.logs),
    case proplists:get_value(App, State#state.logs, CommonLog) of
        undefined ->
            {reply, {error, log_is_not_open}, State};
        Log->
            {reply, log_item(Log, Severity, Item, Caller), State}
    end;
handle_call(_Message, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Message, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, State) ->
    close_all_logs(State#state.logs),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
