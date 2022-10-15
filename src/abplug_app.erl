%%%-------------------------------------------------------------------
%% @doc abplug public API
%% @end
%%%-------------------------------------------------------------------

-module(abplug_app).

-behaviour(application).

-include("abplug.hrl").

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        , prep_stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = abplug_sup:start_link(),
    abplug:load(application:get_all_env()),
    % load_all_drivers(),
    ok = emqx_ctl:register_command(abplug, {abplug_cli, cli}, []),

    {ok, Sup}.

prep_stop(State) ->
    emqx_ctl:unregister_command(abplug),
    % unload_exhooks(),
    unload_all_drivers(),
    State.


stop(_State) ->
    abplug:unload().



%% internal functions

% load_all_drivers() ->
%     load_all_drivers(application:get_env(?APP, drivers, [])).

% load_all_drivers([]) ->
%     ok;
% load_all_drivers([{Name, Opts}|Drivers]) ->
%     ok = emqx_extension_hook:enable(Name, Opts),
%     load_all_drivers(Drivers).

unload_all_drivers() ->
    abplug:disable_all().

search_exhooks() ->
    search_exhooks(ignore_lib_apps(application:loaded_applications())).
search_exhooks(Apps) ->
    lists:flatten([ExHooks || App <- Apps, {_App, _Mod, ExHooks} <- find_attrs(App, exhooks)]).

ignore_lib_apps(Apps) ->
    LibApps = [kernel, stdlib, sasl, appmon, eldap, erts,
               syntax_tools, ssl, crypto, mnesia, os_mon,
               inets, goldrush, gproc, runtime_tools,
               snmp, otp_mibs, public_key, asn1, ssh, hipe,
               common_test, observer, webtool, xmerl, tools,
               test_server, compiler, debugger, eunit, et,
               wx],
    [AppName || {AppName, _, _} <- Apps, not lists:member(AppName, LibApps)].

find_attrs(App, Def) ->
    [{App, Mod, Attr} || {ok, Modules} <- [application:get_key(App, modules)],
                         Mod <- Modules,
                         {Name, Attrs} <- module_attributes(Mod), Name =:= Def,
                         Attr <- Attrs].

module_attributes(Module) ->
    try Module:module_info(attributes)
    catch
        error:undef -> [];
        error:Reason -> error(Reason)
    end.