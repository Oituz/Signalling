-module(cache).
-export([lookup_peer/1,update_peer/2,lookup_sfu/1,update_sfu/2,remove_peer/1,remove_sfu/1]).

-spec lookup_peer(Id::integer())->{ok,Data::map()}|not_found.
lookup_peer(Id)->
    case pg:get_members(Id) of
        [Entry] -> {ok,Entry};
        [] -> not_found
    end.
-spec update_peer(Id::integer(),Pid::pid())->ok.
update_peer(Id,Pid)->
   ok=pg:join(Id, Pid),
   ok.

-spec lookup_sfu(Id::integer())->{ok,Data::map()}|not_found.
lookup_sfu(Id)->
    case mnesia:dirty_read(sfu,Id) of
        [Entry] -> {ok,Entry};
        [] -> not_found
end.
-spec update_sfu(Id::integer(),Data::map())->ok.
update_sfu(Id,Data)->
    mnesia:dirty_write(sfu,{Id,Data}),
    ok.
-spec remove_peer(PeerId::integer())->ok | {error,Error::any()}.
remove_peer(PeerId)->
    Pids=pg:get_members(PeerId),
    pg:leave(PeerId, Pids).

-spec remove_sfu(SFUId::integer())->ok | {error,Error::any()}.
remove_sfu(PeerId)->
    mnesia:dirty_delete(sfu, PeerId).

