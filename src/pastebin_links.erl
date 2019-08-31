-module(pastebin_links).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("recs.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get/1, add/1, exist/1, delete/1, update/1, random_string/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  {ok, Args}.

handle_call({add, Paste}, _From, State) ->
  Transaction = fun() ->
    mnesia:write(Paste)
  end,
  {reply, mnesia:activity(transaction, Transaction), State};
handle_call({delete, #paste_link{hash = Hash} = _Paste}, _From, State) ->
  Transaction = fun() ->
    mnesia:delete({paste_link, Hash})
  end,
  {reply, mnesia:activity(transaction, Transaction), State};
handle_call({get, Link}, _From, State) ->
  Transaction = fun() ->
    mnesia:match_object(#paste_link{name = '$1', link = Link, hash = '$3', type = '$4', path = '$5'})
  end,
  {reply, mnesia:activity(transaction, Transaction), State};
handle_call({exist, Hash}, _From, State) ->
  Transaction = fun() ->
    mnesia:match_object(#paste_link{name = '$1', link = '$2', hash = Hash, type = '$4', path = '$5'})
  end,
  {reply, mnesia:activity(transaction, Transaction), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

random_string(Length) ->
    random:seed(erlang:now()),
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    Rand = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(Rand, "", lists:seq(1, Length)).

exist(Filename) ->
    Hash = checksums:sha256sum(Filename, fast),
    case gen_server:call(?MODULE, {exist, Hash}) of
      [Link] -> Link;
      [] -> false
    end.

add(#paste_link{path = Path} = Link) ->
    Paste = Link#paste_link{
      link = random_string(6),
      hash = checksums:sha256sum(Path, fast)
    },
    gen_server:call(?MODULE, {add, Paste}),
    Paste.

get(Link) ->
    case gen_server:call(?MODULE, {get, Link}) of
      [Paste] ->
        {200, Paste};
      [] ->
        {404, <<"Link not found!\n">>}
    end.

update(#paste_link{link = Link, path = Path} = Paste) ->
  case pastebin_links:get(Link) of
    {200, Old} ->
      {ok, _BytesCopied} = file:copy(Path, Old#paste_link.path),
      Updated = Paste#paste_link{
        path = Old#paste_link.path,
        name = Old#paste_link.name
      },
      ok = gen_server:call(?MODULE, {delete, Old}),
      ok = gen_server:call(?MODULE, {add, Updated}),
      Updated;
    Other -> Other
  end.

delete(Link) ->
  case pastebin_links:get(Link) of
    {200, Old} ->
      ok = file:delete(Old#paste_link.path),
      ok = gen_server:call(?MODULE, {delete, Old}),
      Old;
    Other -> Other
  end.
