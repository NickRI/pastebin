-module(client_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(PLAIN, [{<<"content-type">>, <<"application/x-sh">>}]).

init(_, Req, _Opts) ->
  {ok, Req, _Opts}.

handle(Req, State) ->
  {ok, Client} = client_template:render(pastebin_config:to_template()),
  {ok, RetReq} = cowboy_req:reply(200, makeHeaders(pastebin_config:get_appname(), Client), Client, Req),
  {ok, RetReq, State}.

terminate(_Reason, _Req, _State) ->
	ok.

-spec makeHeaders(iolist(), iolist()) -> [{binary(), binary()}].
makeHeaders(Name, Body) ->
  [
    {<<"content-type">>, <<"application/x-sh">>},
    {<<"content-disposition">>, list_to_binary("attachment; filename=" ++ Name)},
    {<<"content-length">>, integer_to_binary(iolist_size(Body))}
  ].
