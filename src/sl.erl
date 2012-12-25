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

-record(state, {
    format :: fun()
}).

%% ---------------------------------------------------------------------
%% Public methods.
%% ---------------------------------------------------------------------
%% @hidden
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Open specified log file for use.
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
%%        `fun/3' should return `list()' result that represents formatted item.
%%
%%        Default format is: `[DD/MM/YY HH:MM:SS.MIC] PID MODULE (SEVERITY) ...'
%%        </li>
%%      </ul>
-spec open(list(), list()) -> ok | {error, already_open}.
open(LogFile, Opts) ->
    gen_server:call(?MODULE, {open, LogFile, Opts}).

%% @doc Close log and flush it to disk.
-spec close() -> ok | {error, log_is_not_open}.
close() ->
    gen_server:call(?MODULE, close).

%% @doc Log item with `debug' severity.
-spec debug(list()) -> ok | {error, log_is_not_open}.
debug(Item) ->
    log(Item, "debug").

%% @doc Same as {@link debug/1}, but uses io_lib:format to format `Item'
%%      with `Args'.
-spec debug(list(), list()) -> ok | {error, log_is_not_open}.
debug(Item, Args) ->
    log(Item, "debug", Args).

%% @doc Log item with `info' severity.
-spec info(list()) -> ok | {error, log_is_not_open}.
info(Item) ->
    log(Item, "info").

%% @doc Same as {@link info/1}, but uses io_lib:format to format `Item'
%%      with `Args'.
-spec info(list(), list()) -> ok | {error, log_is_not_open}.
info(Item, Args) ->
    log(Item, "info", Args).

%% @doc Log item with `warn' severity.
-spec warn(list()) -> ok | {error, log_is_not_open}.
warn(Item) ->
    log(Item, "warning").

%% @doc Same as {@link warn/1}, but uses io_lib:format to format `Item'
%%      with `Args'.
-spec warn(list(), list()) -> ok | {error, log_is_not_open}.
warn(Item, Args) ->
    log(Item, "warning", Args).

%% @doc Log item with `error' severity.
-spec error(list()) -> ok | {error, log_is_not_open}.
error(Item) ->
    log(Item, "error").

%% @doc Same as {@link error/1}, but uses io_lib:format to format `Item'
%%      with `Args'.
-spec error(list(), list()) -> ok | {error, log_is_not_open}.
error(Item, Args) ->
    log(Item, "error", Args).

%% @doc Log item with `critical' severity.
-spec critical(list()) -> ok | {error, log_is_not_open}.
critical(Item) ->
    log(Item, "critical").

%% @doc Same as {@link critical/1}, but uses io_lib:format to format `Item'
%%      with `Args'.
-spec critical(list(), list()) -> ok | {error, log_is_not_open}.
critical(Item, Args) ->
    log(Item, "critical", Args).

%% @doc Command method for logging with custom `Severity'.
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
format_default(Severity, Item, {CallerPid, CallerMod}) ->
    io_lib:format("[~s] ~11s ~-10s (~8ts) ~ts~n", [
        format_date(),
        pid_to_list(CallerPid), CallerMod,
        string:to_upper(Severity), Item
    ]).
parse_opts(Opts) ->
    #state{
        format = proplists:get_value(format, Opts, fun format_default/3)
    }.

open_log(State, LogFile) ->
    {ok, ?MODULE} = disk_log:open([
        {name, ?MODULE},
        {file, LogFile},
        {format, external}
    ]),
    ok = log_item(State#state.format, "info", "log started", get_caller()),
    ok.

log_item(Format, Severity, Item, Caller) ->
    disk_log:balog(?MODULE, Format(Severity, Item, Caller)).

close_log(State) ->
    ok = log_item(State#state.format, "info", "log shutdown", get_caller()),
    case disk_log:close(?MODULE) of
        ok ->
            ok;
        _ ->
            {error, log_is_not_open}
    end.

get_caller() ->
    CallerPid = self(),
    CallerMod = element(1, lists:nth(3, element(2, element(2,
        catch erlang:error([]))))),
    {CallerPid, CallerMod}.

%% ---------------------------------------------------------------------
%% gen_server specific.
%% ---------------------------------------------------------------------
%% @hidden
init(_Args) ->
    {ok, nil}.

%% @hidden
handle_call({open, LogFile, Opts}, _From, nil) ->
    State = parse_opts(Opts),
    Result = open_log(State, LogFile),
    {reply, Result, State};
handle_call({open, _, _}, _From, State) ->
    {reply, {error, already_open}, State};
handle_call(close, _From, nil) ->
    {reply, {error, log_is_not_open}, nil};
handle_call(close, _From, State) ->
    Result = close_log(State),
    {reply, Result, nil};
handle_call({log, _, _, _}, _From, nil) ->
    {reply, {error, log_is_not_open}, nil};
handle_call({log, Severity, Item, Caller}, _From, State) ->
    Result = log_item(State#state.format, Severity, Item, Caller),
    {reply, Result, State};
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
    close_log(State),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
