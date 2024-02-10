-module(cache).
-export([lookup_peer/1,update_peer/2,lookup_sfu/1,update_sfu/2]).


lookup_peer(Id)->
    case mnesia:dirty_read(peers,Id) of
        [Entry] -> {ok,Entry};
        [] -> not_found
end.

update_peer(Id,Data)->
    mnesia:dirty_write(peers,{Id,Data}),
    ok.

lookup_sfu(Id)->
    case mnesia:dirty_read(sfus,Id) of
        [Entry] -> {ok,Entry};
        [] -> not_found
end.

update_sfu(Id,Data)->
    mnesia:dirty_write(sfus,{Id,Data}),
    ok.



