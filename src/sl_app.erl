%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Simple Logger Application.
%% @private
-module(sl_app).
-created('Date: 25/12/2012').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').
-behaviour(application).

-export([start/2, stop/1]).

%% ---------------------------------------------------------------------
%% Public methods.
%% ---------------------------------------------------------------------
%% @doc Starts an application.
start(_Type, _Args) ->
	sl_sup:start_link().

%% @doc Stops an application.
stop(_State) ->
	ok.
