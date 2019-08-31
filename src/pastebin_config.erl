-module(pastebin_config).

-export([
  get_hostname/0,
  get_appname/0,
  get_port/0,
  get_key/1,
  get_full_hostname/0,
  get_full_hostname/1,
  generate_link_uri/1,
  to_template/0
]).

to_template() ->
  [
    {appname, get_appname()},
    {hostname, get_hostname()},
    {port, get_proxy_port()},
    {version, get_vsn()},
    {full_hostname, get_full_hostname()},
    {if_hostname, get_full_hostname(false)}
  ].

get_host_by_interface() ->
  {ok, [ {IP, _Broadcast, _NetMask} | _ ]} = inet:getif(),
  {ok, {hostent, Hostname, _ ,inet, _Family, _Addresses}} = inet:gethostbyaddr(IP),
  Hostname.

get_full_hostname() ->
  case get_hostname() of
    false ->
      get_full_hostname(false);
    true ->
      get_full_hostname(false);
    undefined ->
      get_full_hostname(false);
    _ ->
      get_full_hostname(true)
  end.

get_full_hostname(ByConfig) ->
  Port = get_proxy_port(),
  Hostname = if ByConfig == false -> get_host_by_interface(); true -> get_hostname() end,
  string:join(["http:/", Hostname], "/") ++ if Port == 80 -> ""; true -> ":" ++ integer_to_list(Port) end.

generate_link_uri(LinkId) ->
  string:join([get_full_hostname(), LinkId],  "/") ++ ["\n"].

get_hostname() ->
  get_key(hostname).

get_appname() ->
  get_key(appname).

get_proxy_port() ->
  get_key(proxyport, get_key(port)).

get_port() ->
  get_key(port).

get_vsn() ->
  {ok, Vsn} = application:get_key(pastebin, vsn),
  Vsn.

get_key(Key) ->
  proplists:get_value(Key, application:get_all_env(pastebin)).

get_key(Key, Default) ->
  proplists:get_value(Key, application:get_all_env(pastebin), Default).
