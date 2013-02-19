%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc General End User API.
-module(sl).
-created('Date: 25/12/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([install/1, install/2]).
-export([open/1, open/2, close/0, log/2, log/3, log/4]).
-export([
    debug/1, debug/2, debug/3,
    info/1, info/2, info/3,
    warn/1, warn/2, warn/3,
    error/1, error/2, error/3,
    crit/1, crit/2, crit/3
]).

%% @doc Open specified log file for use and make it available
%%      for current application.
%%
%%      Currently only one log per application and one top level
%%      log is supported.
%%
%%      Top level log is a log that opened without application context.
%%
%%      Application name obtained via `application:get_application/0' call.
-spec open(list()) -> ok | {error, term()}.
open(LogFile) ->
    open(LogFile, []).

%% @doc Same as {@link open/1}, but with additional opts.
%%      `Opts' is a proplist with keys:
%%      <ul>
%%      <li>`format' specifies `fun/2' that should format incoming item before
%%        writing to log
%%
%%        `fun/2' must have 2 arguments: `Severity', `ItemToLog'.
%%
%%        `fun/2' should return `list()' result that represents formatted item.
%%
%%        <b>Caution</b>: formatting function will be called in context of caller
%%        module, so any error in formatting function can crash module itself.
%%
%%        Default format is: `[Day/Mon/Year Hour:Min:Sec.Micro] Pid Module (Severity) ...'
%%        </li>
%%      </ul>
-spec open(string(), list(atom() | tuple())) -> ok | {error, term()}.
open(LogFile, Opts) ->
    sl_sup:spawn_worker(get_caller_app(), LogFile, Opts).

%% @doc Install log to caller application to use, if posible.
%%
%%      Shorthand for:
%%      ```
%%      case application:get_env(EnvName) of
%%          undefined -> ok;
%%          {ok, FileName} -> open(FileName, Opts)
%%      end.
%%      '''
%%
%%      `EnvName' is stands for env variable name to take log file name from.
install(EnvName) ->
    install(EnvName, []).

%% @doc Same as {@link install/1}, but with custom `Opts' passed
%%      to {@link open/2}.
install(EnvName, Opts) ->
    case application:get_env(EnvName) of
        undefined -> ok;
        {ok, FileName} -> open(FileName, Opts)
    end.
 
%% @doc Close log and flush it to disk.
-spec close() -> ok | {error, term()}.
close() ->
    case sl_sup:kill_worker(get_caller_app()) of
        {error, simple_one_for_one} -> {error, noproc};
        Result -> Result
    end.

%% @doc Log item with `debug' severity. See {@link log/2} for more info.
-spec debug(string()) -> ok | {error, term()}.
debug(Item) ->
    log(Item, "debug").

%% @doc Same as {@link debug/1}, but uses io_lib:format to format `Item'
%%      with `Args'. See {@link log/2} for more info.
-spec debug(string(), list()) -> ok | {error, term()}.
debug(Item, Args) ->
    log(Item, "debug", Args).

%% @doc Same as {@link debug/2}, but allows to specify log id (e.g. application
%%      name to log to).
-spec debug(atom(), string(), list()) -> ok | {error, term()}.
debug(Log, Item, Args) ->
    log(Log, Item, "debug", Args).

%% @doc Log item with `info' severity. See {@link log/2} for more info.
-spec info(string()) -> ok | {error, term()}.
info(Item) ->
    log(Item, "info").

%% @doc Same as {@link info/1}, but uses io_lib:format to format `Item'
%%      with `Args'. See {@link log/2} for more info.
-spec info(string(), list()) -> ok | {error, term()}.
info(Item, Args) ->
    log(Item, "info", Args).

%% @doc Same as {@link info/2}, but allows to specify log id (e.g. application
%%      name to log to).
-spec info(atom(), string(), list()) -> ok | {error, term()}.
info(Log, Item, Args) ->
    log(Log, Item, "info", Args).

%% @doc Log item with `warn' severity. See {@link log/2} for more info.
-spec warn(string()) -> ok | {error, term()}.
warn(Item) ->
    log(Item, "warn").

%% @doc Same as {@link warn/1}, but uses io_lib:format to format `Item'
%%      with `Args'. See {@link log/2} for more info.
-spec warn(string(), list()) -> ok | {error, term()}.
warn(Item, Args) ->
    log(Item, "warn", Args).

%% @doc Same as {@link warn/2}, but allows to specify log id (e.g. application
%%      name to log to).
-spec warn(atom(), string(), list()) -> ok | {error, term()}.
warn(Log, Item, Args) ->
    log(Log, Item, "warn", Args).

%% @doc Log item with `error' severity. See {@link log/2} for more info.
-spec error(string()) -> ok | {error, term()}.
error(Item) ->
    log(Item, "error").

%% @doc Same as {@link error/1}, but uses io_lib:format to format `Item'
%%      with `Args'. See {@link log/2} for more info.
-spec error(string(), list()) -> ok | {error, term()}.
error(Item, Args) ->
    log(Item, "error", Args).

%% @doc Same as {@link error/2}, but allows to specify log id (e.g. application
%%      name to log to).
-spec error(atom(), string(), list()) -> ok | {error, term()}.
error(Log, Item, Args) ->
    log(Log, Item, "error", Args).

%% @doc Log item with `crit' severity. See {@link log/2} for more info.
-spec crit(string()) -> ok | {error, term()}.
crit(Item) ->
    log(Item, "crit").

%% @doc Same as {@link crit/1}, but uses io_lib:format to format `Item'
%%      with `Args'. See {@link log/2} for more info.
-spec crit(string(), list()) -> ok | {error, term()}.
crit(Item, Args) ->
    log(Item, "crit", Args).

%% @doc Same as {@link crit/2}, but allows to specify log id (e.g. application
%%      name to log to).
-spec crit(atom(), string(), list()) -> ok | {error, term()}.
crit(Log, Item, Args) ->
    log(Log, Item, "crit", Args).

%% @doc Common method for logging with custom `Severity'.
%%      This call will automatically determines what log file it should use
%%      in current application (obtained via `application:get_application/0').
-spec log(string(), string()) -> ok | {error, term()}.
log(Item, Severity) ->
    log(Item, Severity, []).

%% @doc Same as {@link log/2}, but uses io_lib:format to format `Item'
%%      with `Args'.
-spec log(string(), string(), list()) -> ok | {error, term()}.
log(Item, Severity, Args) when is_list(Args) ->
    log(get_caller_app(), Item, Severity, Args).

%% @doc Same as {@link crit/2}, but allows to specify log id (e.g. application
%%      name to log to).
-spec log(atom(), string(), string(), list()) -> ok | {error, term()}.
log(Log, Item, Severity, Args) ->
    try
        sl_sup:call_worker(Log,
            {log, {Severity, format(Log, Severity, Item, Args)}})
    catch
        exit:{noproc, _} -> {error, noproc}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_caller_app() -> atom().
get_caller_app() ->
    case application:get_application() of
        {ok, Name} ->
            Name;
        _ ->
            kernel
    end.

-spec format(atom(), string(), string(), list()) -> binary().
format(App, Severity, Item, Args) ->
    Opts = sl_worker:get_opts(App),
    Format = proplists:get_value(format, Opts, fun format_default/3),
    iolist_to_binary(Format(Severity, Item, Args)).

-spec format_date() -> iolist().
format_date() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
    {_, _, Microsec} = now(),
    io_lib:format("~2..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B.~3..0B", [
        Day, Month, Year rem 1000,
        Hour, Minute, Second,
        Microsec div 1000]).

-spec format_default(string(), string(), list()) -> iolist().
format_default(Severity, Item, Args) ->
    [$[, format_date(), $], " ",              % date
        io_lib:format("~11s ~-10s (~-5ts)", [ % pid, mod, severity
            pid_to_list(self()),
            get_caller_mod(),
            string:to_upper(Severity)]), " ",
        io_lib:format(Item, Args), $\n].      % message

-spec get_caller_mod() -> atom().
get_caller_mod() ->
    element(1, lists:nth(5, element(2, element(2, catch erlang:error([]))))).
