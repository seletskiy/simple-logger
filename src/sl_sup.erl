%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Main application supervisor.
%% @private
-module(sl_sup).
-created('Date: 25/12/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% ---------------------------------------------------------------------
%% Public methods.
%% ---------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [[]]).

init(_Args) ->
    {ok, {{one_for_one, 5, 60}, [
        {sl,
            {sl, start_link, []},
            permanent,
            5000,
            worker,
            [sl]}
    ]}}.
