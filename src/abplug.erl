-module(abplug).

-export([load/1
    % , unload/0
]).
-include("emqx_extension_hook.hrl").
% -include_lib("emqx/include/emqx.hrl").
% -include_lib("emqx/include/emqx_hooks.hrl").

% % %% for logging
% -include_lib("emqx/include/logger.hrl").

-record(message,{id, qos, from, topic , payload, timestamp }).

%% Client Lifecircle Hooks
-export([ 
        on_client_connect/2
        , on_client_connack/4
        , on_client_connected/3
        , on_client_disconnected/4
        , on_client_authenticate/3
        , on_client_authorize/5
        , on_client_subscribe/4
        , on_client_unsubscribe/4
        ]).

%% Utils
-export([ message/1
        , validator/1
        , assign_to_message/2
        , test/1
        , clientinfo/1
        , stringfy/1
        ]).

-import(emqx_extension_hook,
        [ cast/2
        , call_fold/4
        ]).

%% Session Lifecircle Hooks
-export([ on_session_created/3
        , on_session_subscribed/4
        , on_session_unsubscribed/4
        , on_session_resumed/3
        , on_session_discarded/3
        , on_session_takeovered/3
        , on_session_terminated/4
        ]).

% Message Pubsub Hooks
-export([ on_message_publish/2
        , on_message_delivered/3
        , on_message_acked/3
        , on_message_dropped/4
        ]).

load(Env) ->
    emqx:hook('client.connect',      {?MODULE, on_client_connect, [Env]}),
    emqx:hook('client.connack',      {?MODULE, on_client_connack, [Env]}),
    emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    emqx:hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
    emqx:hook('client.check_acl',    {?MODULE, on_client_check_acl, [Env]}),
    emqx:hook('client.subscribe',    {?MODULE, on_client_subscribe, [Env]}),
    emqx:hook('client.unsubscribe',  {?MODULE, on_client_unsubscribe, [Env]}),
    emqx:hook('session.created',     {?MODULE, on_session_created, [Env]}),
    emqx:hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    emqx:hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
    emqx:hook('session.resumed',     {?MODULE, on_session_resumed, [Env]}),
    emqx:hook('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
    emqx:hook('session.takeovered',  {?MODULE, on_session_takeovered, [Env]}),
    emqx:hook('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
    emqx:hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
    emqx:hook('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
    emqx:hook('message.acked',       {?MODULE, on_message_acked, [Env]}),
    emqx:hook('message.dropped',     {?MODULE, on_message_dropped, [Env]}).


%%--------------------------------------------------------------------
%% Client LifeCircle Hooks
%%--------------------------------------------------------------------

% on_client_connect(ConnInfo, Props, _Env) ->
%     %% this is to demo the usage of EMQX's structured-logging macro
%     %% * Recommended to always have a `msg` field,
%     %% * Use underscore instead of space to help log indexers,
%     %% * Try to use static fields
%     ?SLOG(debug, #{msg => "demo_log_msg_on_client_connect",
%                    conninfo => ConnInfo,
%                    props => Props}),
%     {ok, Props}.

on_client_connect(ConnInfo, _Props) ->
    cast('client_connect', [conninfo(ConnInfo), props(_Props)]).

on_client_connack(ConnInfo = #{clientid := ClientId}, Rc, Props, _Env) ->
    io:format("Client(~s) connack, ConnInfo: ~p, Rc: ~p, Props: ~p~n",
              [ClientId, ConnInfo, Rc, Props]),
    {ok, Props}.

on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
    io:format("Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ClientInfo, ConnInfo]).

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
  io:format("Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ReasonCode, ClientInfo, ConnInfo]).

on_client_authenticate(ClientInfo = #{clientid := ClientId}, Result, Env) ->
  io:format("Client(~s) authenticate, ClientInfo:~n~p~n, Result:~p,~nEnv:~p~n",
    [ClientId, ClientInfo, Result, Env]),
  {ok, Result}.

on_client_authorize(ClientInfo = #{clientid := ClientId}, PubSub, Topic, Result, Env) ->
  io:format("Client(~s) authorize, ClientInfo:~n~p~n, ~p to topic(~s) Result:~p,~nEnv:~p~n",
    [ClientId, ClientInfo, PubSub, Topic, Result, Env]),
  {ok, Result}.

on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    io:format("Client(~s) will subscribe: ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.

on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    io:format("Client(~s) will unsubscribe ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.

%%--------------------------------------------------------------------
%% Session LifeCircle Hooks
%%--------------------------------------------------------------------

on_session_created(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) created, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, _Env) ->
    io:format("Session(~s) subscribed ~s with subopts: ~p~n", [ClientId, Topic, SubOpts]).

on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, _Env) ->
    io:format("Session(~s) unsubscribed ~s with opts: ~p~n", [ClientId, Topic, Opts]).

on_session_resumed(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) resumed, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_discarded(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is discarded. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_takeovered(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is takeovered. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_terminated(_ClientInfo = #{clientid := ClientId}, Reason, SessInfo, _Env) ->
    io:format("Session(~s) is terminated due to ~p~nSession Info: ~p~n",
              [ClientId, Reason, SessInfo]).

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------

%% Transform message and return
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

on_message_publish(Message = #message{payload = <<"netstratum">>}, _Env) ->
    io:format("Welcome to Nestratum"),
    lager:info("Welcome to Nestratum1"),
    {ok, Message};

on_message_publish(Message, _Env) ->
    io:format("Publish ~p~n", [emqx_message:to_map(Message)]),
    {ok, Message}.

on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason, _Env) ->
    ok;
on_message_dropped(Message, _By = #{node := Node}, Reason, _Env) ->
    io:format("Message dropped by node ~p due to ~p:~n~p~n",
              [Node, Reason, emqx_message:to_map(Message)]).

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format("Message delivered to client(~s):~n~p~n",
              [ClientId, emqx_message:to_map(Message)]),
    {ok, Message}.

on_message_acked(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format("Message acked by client(~s):~n~p~n",
              [ClientId, emqx_message:to_map(Message)]).

%% Called when the plugin application stop
% unload() ->
%     unhook('client.connect',      {?MODULE, on_client_connect}),
%     unhook('client.connack',      {?MODULE, on_client_connack}),
%     unhook('client.connected',    {?MODULE, on_client_connected}),
%     unhook('client.disconnected', {?MODULE, on_client_disconnected}),
%     unhook('client.authenticate', {?MODULE, on_client_authenticate}),
%     unhook('client.authorize',    {?MODULE, on_client_authorize}),
%     unhook('client.check_acl',    {?MODULE, on_client_check_acl}),
%     unhook('client.subscribe',    {?MODULE, on_client_subscribe}),
%     unhook('client.unsubscribe',  {?MODULE, on_client_unsubscribe}),
%     unhook('session.created',     {?MODULE, on_session_created}),
%     unhook('session.subscribed',  {?MODULE, on_session_subscribed}),
%     unhook('session.unsubscribed',{?MODULE, on_session_unsubscribed}),
%     unhook('session.resumed',     {?MODULE, on_session_resumed}),
%     unhook('session.discarded',   {?MODULE, on_session_discarded}),
%     unhook('session.takeovered',  {?MODULE, on_session_takeovered}),
%     unhook('session.terminated',  {?MODULE, on_session_terminated}),
%     unhook('message.publish',     {?MODULE, on_message_publish}),
%     unhook('message.delivered',   {?MODULE, on_message_delivered}),
%     unhook('message.acked',       {?MODULE, on_message_acked}),
%     unhook('message.dropped',     {?MODULE, on_message_dropped}).

% hook(HookPoint, MFA) ->
%     %% use highest hook priority so this module's callbacks
%     %% are evaluated before the default hooks in EMQX
%     emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

% unhook(HookPoint, MFA) ->
%     emqx_hooks:del(HookPoint, MFA).




props(undefined) -> [];
props(M) when is_map(M) -> maps:to_list(M).

conninfo(_ConnInfo =
         #{clientid := ClientId, username := Username, peername := {Peerhost, _},
           sockname := {_, SockPort}, proto_name := ProtoName, proto_ver := ProtoVer,
           keepalive := Keepalive}) ->
    [{node, node()},
     {clientid, ClientId},
     {username, maybe(Username)},
     {peerhost, ntoa(Peerhost)},
     {sockport, SockPort},
     {proto_name, ProtoName},
     {proto_ver, ProtoVer},
     {keepalive, Keepalive}].

clientinfo(ClientInfo =
           #{clientid := ClientId, username := Username, peerhost := PeerHost,
             sockport := SockPort, protocol := Protocol, mountpoint := Mountpoiont}) ->
    [{node, node()},
     {clientid, ClientId},
     {username, maybe(Username)},
     {password, maybe(maps:get(password, ClientInfo, undefined))},
     {peerhost, ntoa(PeerHost)},
     {sockport, SockPort},
     {protocol, Protocol},
     {mountpoint, maybe(Mountpoiont)},
     {is_superuser, maps:get(is_superuser, ClientInfo, false)},
     {anonymous, maps:get(anonymous, ClientInfo, true)}].

message(#message{id = Id, qos = Qos, from = From, topic = Topic, payload = Payload, timestamp = Ts}) ->
    [{node, node()},
     {id, hexstr(Id)},
     {qos, Qos},
     {from, From},
     {topic, Topic},
     {payload, Payload},
     {timestamp, Ts}].

topicfilters(Tfs = [{_, _}|_]) ->
    [{Topic, Qos} || {Topic, #{qos := Qos}} <- Tfs];
topicfilters(Tfs) ->
    Tfs.

ntoa({0,0,0,0,0,16#ffff,AB,CD}) ->
    list_to_binary(inet_parse:ntoa({AB bsr 8, AB rem 256, CD bsr 8, CD rem 256}));
ntoa(IP) ->
    list_to_binary(inet_parse:ntoa(IP)).

maybe(undefined) -> <<"">>;
maybe(B) -> B.

%% @private
stringfy(Term) when is_binary(Term) ->
    Term;
stringfy(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
stringfy(Term) when is_tuple(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

hexstr(B) ->
    iolist_to_binary([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(B)]).

%%--------------------------------------------------------------------
%% Validator funcs

validator(Name) ->
    fun(V) -> validate_acc_arg(Name, V) end.

validate_acc_arg('client_authenticate', V) when is_boolean(V) -> true;
validate_acc_arg('client_check_acl',    V) when is_boolean(V) -> true;
validate_acc_arg('message_publish',     V) when is_list(V) -> validate_msg(V, true);
validate_acc_arg(_,                     _) -> false.

validate_msg([], Bool) ->
    Bool;
validate_msg(_, false) ->
    false;
validate_msg([{topic, T} | More], _) ->
    validate_msg(More, is_binary(T));
validate_msg([{payload, P} | More], _) ->
    validate_msg(More, is_binary(P));
validate_msg([{qos, Q} | More], _) ->
    validate_msg(More, Q =< 2 andalso Q >= 0);
validate_msg([{timestamp, T} | More], _) ->
    validate_msg(More, is_integer(T));
validate_msg([_ | More], _) ->
    validate_msg(More, true).

%%--------------------------------------------------------------------
%% Misc

assign_to_message([], Message) ->
    Message;
assign_to_message([{topic, Topic}|More], Message) ->
    assign_to_message(More, Message#message{topic = Topic});
assign_to_message([{qos, Qos}|More], Message) ->
    assign_to_message(More, Message#message{qos = Qos});
assign_to_message([{payload, Payload}|More], Message) ->
    assign_to_message(More, Message#message{payload = Payload});
assign_to_message([_|More], Message) ->
    assign_to_message(More, Message).


test(Message) when payload =:= <<"netstratum">> ->
  lager:info("welcome to Netsrtatum3").
