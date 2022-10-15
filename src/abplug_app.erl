%%%-------------------------------------------------------------------
%% @doc abplug public API
%% @end
%%%-------------------------------------------------------------------

-module(abplug_app).

-behaviour(application).


-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = abplug_sup:start_link(),
    abplug:load(application:get_all_env()),
    {ok, Sup}.

stop(_State) ->
    abplug:unload().


%% internal functions
