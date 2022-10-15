-module(abplug_cli).

-export([cmd/1]).

cmd(["arg1", "arg2"]) ->
    emqx_cli:print ("ok");

cmd(_) ->
    emqx_cli:usage ([{"cmd arg1 arg2", "cmd demo"}]).