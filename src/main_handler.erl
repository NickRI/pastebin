-module(main_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("include/recs.hrl").

-define(PLAIN, [{<<"content-type">>, <<"text/plain">>}]).
-define(HTML, [{<<"content-type">>, <<"text/html">>}]).


-record(state, {
  paste_id
}).

-record(data, {
  value
}).

-record(file, {
  name,
  type,
  encoding,
  temp
}).

init(_, Req, _Opts) ->
  {PasteId, Req2} = cowboy_req:binding(paste_id, Req, none),

  {ok, Req2, #state{
    paste_id = PasteId
  }}.

handle(Req, State) ->
  case cowboy_req:method(Req) of
    {<<"GET">>, Req2} ->
      handle_get(Req2, State);
    {<<"POST">>, Req2} ->
      try handle_post(Req2, State) of
        Results -> Results
      catch
        file_param_not_found ->
          {ok, Req3} = cowboy_req:reply(400, ?PLAIN, <<"Needed 'file' param\n">>, Req2),
          {ok, Req3, State}
      end;
    {<<"PUT">>, Req2} ->
      handle_put(Req2, State);
    {<<"DELETE">>, Req2} ->
      handle_delete(Req2, State)
  end.

terminate(_Reason, _Req, _State) ->
	ok.

-spec handle_get(cowboy_req:req(), #state{}) -> {ok, cowboy_req:req(), #state{}}.
handle_get(Req, State) ->
  {ok, NewReq} = case State#state.paste_id of
    none ->
      {ok, Help} = help_template:render(pastebin_config:to_template()),
    	cowboy_req:reply(200, ?HTML, Help, Req);
    PasteId ->
      case pastebin_links:get(erlang:bitstring_to_list(PasteId)) of
        {200, Paste} ->
          Sender = fun(Socket, Transport) ->
            Transport:sendfile(Socket, Paste#paste_link.path)
          end,
          ReqBodyFun = cowboy_req:set_resp_body_fun(filelib:file_size(Paste#paste_link.path), Sender, Req),
          cowboy_req:reply(200, makeHeaders(Paste), ReqBodyFun);
        {Code, Body} ->
          cowboy_req:reply(Code, ?PLAIN, Body, Req)
      end
  end,
	{ok, NewReq, State}.

-spec stream_file(cowboy_req:req(), file:io_device()) -> {ok, cowboy_req:req()}.
stream_file(Req, IoDevice) ->
  case cowboy_req:part_body(Req) of
    {ok, Chunk, Req2} ->
        ok = file:write(IoDevice, Chunk),
        {ok, Req2};
    {more, Chunk, Req2} ->
        ok = file:write(IoDevice, Chunk),
        stream_file(Req2, IoDevice)
  end.

-type fieldType() :: #data{} | #file{}.
-type field() :: {[byte()], fieldType()}.

-spec multipart(cowboy_req:req()) -> {ok, cowboy_req:req(), [field()]}.
multipart(Req) -> multipart(Req, []).

-spec multipart(cowboy_req:req(), [proplists:property()]) -> {ok, cowboy_req:req(), [field()]}.
multipart(Req, Options) -> multipart(Req, Options, []).

-spec multipart(cowboy_req:req(), [proplists:property()], [field()]) -> {ok, cowboy_req:req(), [field()]}.
multipart(Req, Options, Fields) ->
    case cowboy_req:part(Req) of
      {ok, Headers, Req2} ->
          case cow_multipart:form_data(Headers) of
            {data, FieldName} ->
              {ok, Value, Req3} = cowboy_req:part_body(Req2),
              Field = {binary_to_list(FieldName), #data{ value= binary_to_list(Value) } };
            {file, FieldName, Filename, Type, Encoding} ->
              TempPath = temp_file_name(Options),
              {ok, IoDevice} = file:open(TempPath, [write]),
              {ok, Req3} = stream_file(Req2, IoDevice),
              ok = file:close(IoDevice),
              Field = {binary_to_list(FieldName), #file{
                name = binary_to_list(Filename),
                temp = TempPath,
                encoding = binary_to_list(Encoding),
                type = binary_to_list(Type)
              }}
          end,
          multipart(Req3, Options, [Field | Fields]);
      {done, Req2} ->
        {ok, Req2, Fields}
    end.

-spec handle_post(cowboy_req:req(), #state{}) -> {ok, cowboy_req:req(), #state{}}.
handle_post(Req, State) ->
  {ok, Req2, Fields} = multipart(Req),
  case proplists:is_defined("file", Fields) of
    false -> throw(file_param_not_found);
    true -> ok
  end,
  case proplists:get_value("file", Fields) of
    #file{name=Name, type=_Type, encoding= _Encoding, temp = Tempname} ->
      Paste = case pastebin_links:exist(Tempname) of
        #paste_link{} = Link -> Link;
        false ->
          Path = movetemp(Tempname),
          pastebin_links:add(#paste_link{name = Name, type = get_mime_type(Path), path = Path})
      end;
    #data{value = Val} ->
      Tempname = temp_file_name(),
      ok = file:write_file(Tempname, Val),
      Paste = case pastebin_links:exist(Tempname) of
        #paste_link{} = Link -> Link;
        false ->
          Path = movetemp(Tempname),
          {data, Name} = proplists:get_value("name", Fields, {data, pastebin_links:random_string(6)}),
          pastebin_links:add(#paste_link{name = Name, type = get_mime_type(Path), path = Path})
      end
  end,
  {ok, Req3} = cowboy_req:reply(200, ?PLAIN, pastebin_config:generate_link_uri(Paste#paste_link.link), Req2),
  {ok, Req3, State}.

-spec stream_body(cowboy_req:req(), file:io_device()) -> {ok, cowboy_req:req()}.
stream_body(Req, IoDevice) ->
  case cowboy_req:body(Req) of
    {ok, Chunk, Req2} ->
      ok = file:write(IoDevice, Chunk),
      {ok, Req2};
    {more, Chunk, Req2} ->
      ok = file:write(IoDevice, Chunk),
      stream_body(Req2, IoDevice)
  end.

-spec handle_put(cowboy_req:req(), #state{}) -> {ok, cowboy_req:req(), #state{}}.
handle_put(Req, State) ->
  Link = State#state.paste_id,
  Tempname = temp_file_name(),
  {ok, IoDevice} = file:open(Tempname, [write]),
  {ok, Req2} = stream_body(Req, IoDevice),
  ok = file:close(IoDevice),

  case pastebin_links:update(#paste_link{
    link = bitstring_to_list(Link),
    type = get_mime_type(Tempname),
    hash = checksums:sha256sum(Tempname, fast),
    path = Tempname
  }) of
    {Code, Msg} ->
      {ok, NewReq} = cowboy_req:reply(Code, ?PLAIN, Msg, Req2);
    Paste ->
      {ok, NewReq} = cowboy_req:reply(200, ?PLAIN, pastebin_config:generate_link_uri(Paste#paste_link.link), Req2)
  end,
  {ok, NewReq, State}.

-spec handle_delete(cowboy_req:req(), #state{}) -> {ok, cowboy_req:req(), #state{}}.
handle_delete(Req, State) ->
  case pastebin_links:delete(bitstring_to_list(State#state.paste_id)) of
    {Code, Msg} ->
      {ok, NewReq} = cowboy_req:reply(Code, ?PLAIN, Msg, Req);
    Paste ->
      {ok, NewReq} = cowboy_req:reply(200, ?PLAIN, list_to_binary(Paste#paste_link.link ++ " is deleted\n"), Req)
  end,
  {ok, NewReq, State}.

-type bitString() :: byte() | bitstring().

-spec makeHeaders(#paste_link{}) -> [{bitString(), bitString()}].
makeHeaders(#paste_link{name = Name, type = Type, path = Path}) ->
  [
    {<<"content-type">>, Type},
    {<<"content-disposition">>, list_to_binary("attachment; filename=" ++ Name)},
    {<<"content-length">>, integer_to_binary(filelib:file_size(Path))}
  ].

-spec temp_file_name() -> [bitString()].
temp_file_name() -> temp_file_name([]).

-spec temp_file_name([proplists:property()]) -> [bitString()].
temp_file_name(Options) ->
  string:join([proplists:get_value(dest, Options, "/tmp"), pastebin_links:random_string(8)], "/") ++ proplists:get_value(ext, Options, ".tmp").

-spec get_mime_type([byte()]) -> bitstring().
get_mime_type(File) ->
  list_to_binary(lists:droplast(os:cmd("file --mime-type -b " ++ File))).

-spec movetemp([byte()]) -> [bitString()].
movetemp(Tempname) ->
  Filename = temp_file_name([{dest, "./files"}, {ext, ".store"}]),
  ok = file:rename(Tempname, Filename),
  Filename.
