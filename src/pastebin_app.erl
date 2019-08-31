-module(pastebin_app).

-behaviour(application).

%% Application callbacks

-export([start/2, stop/1, install_mnesia/0, install_mnesia/1, start_dev/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-include("recs.hrl").

install_mnesia() ->
  install_mnesia([node() | nodes()]).

install_mnesia(Nodes) ->
  rpc:multicall(Nodes, application, stop, [mnesia]),
  case mnesia:create_schema(Nodes) of
    {error, { CurNode, {already_exists, Node} } } ->
      io:format("[~p]: Allready axist for ~p node~n", [CurNode, Node]);
    _ ->
      io:format("Normall added~n", [])
  end,
  rpc:multicall(Nodes, application, start, [mnesia]),
  mnesia:create_table(paste_link, [
    {attributes, record_info(fields, paste_link)},
    {index, [#paste_link.link, #paste_link.name]},
    {disc_copies, Nodes}
  ]).

start_dev() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowlib),
  application:start(cowboy),
  application:start(compiler),
  application:start(syntax_tools),
  application:start(merl),
  application:start(erlydtl),
  application:start(systools),
  application:start(mnesia),
  application:start(pastebin).


start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
          {"/favicon.ico", cowboy_static, {priv_file, pastebin, "favicon.ico"}},
          {"/vendor/[...]", cowboy_static, {dir, "priv/vendor"}},
          {"/client", client_handler, []},
          {"/[:paste_id]", main_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_http(http_listener, 100, [{port, pastebin_config:get_port()}],
        [{env, [{dispatch, Dispatch}]}]
    ),

    {ok, help_template} = erlydtl:compile(filename:join(code:priv_dir(pastebin), "help.dtl"), help_template, [{out_dir, false}]),
    {ok, client_template} = erlydtl:compile(filename:join(code:priv_dir(pastebin), "client.dtl"), client_template, [{out_dir, false}]),

    case mnesia:wait_for_tables([paste_link], 3000) of
      ok ->
        io:format("Mnesia is successfuly loaded~n");
      {timeout, Tables} ->
        io:format("Tables ~p cant be loaded, timeout~n", [Tables]);
      {error, Error} ->
        io:format("The error is occured ~p~n", [Error])
    end,
    pastebin_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener).
