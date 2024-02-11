-module(cache).
-export([lookup_peer/1,update_peer/2,lookup_sfu/1,update_sfu/2,remove_peer/1,remove_sfu/1]).

-spec lookup_peer(Id::integer())->{ok,Data::map()}|not_found.
lookup_peer(Id)->
    case mnesia:dirty_read(peer,Id) of
        [Entry] -> {ok,Entry};
        [] -> not_found
end.
-spec update_peer(Id::integer(),Data::map())->ok.
update_peer(Id,Data)->
    mnesia:dirty_write(peer,{Id,Data}),
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
    mnesia:dirty_delete(peer, PeerId).

-spec remove_sfu(SFUId::integer())->ok | {error,Error::any()}.
remove_sfu(PeerId)->
    mnesia:dirty_delete(sfu, PeerId).


