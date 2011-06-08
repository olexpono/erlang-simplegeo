-module(simplegeo).

-export([start/0]).
-export([get_record/2, get_records/2, add_record/1]).
-export([get_history/2, get_nearby_geohash/2, get_nearby/3]).
-export([get_nearby_address/2, get_density_day/3, get_density_hour/3]).
-export([get_layer/1, get_contains/2, get_overlaps/4, get_boundary/1]).

start() ->
    application:start(simplegeo).

get_record(Layer, Id) ->
    do_get(record, [{layer, Layer}, {id, Id}]).

get_records(Layer, Ids) when is_list(Ids) ->
    do_get(records, [{layer, Layer}, {ids, string:join(Ids, ",")}]).

add_record({struct, Props}) ->
    {_, Layer} = lists:keyfind(<<"layer">>, 1, Props),
    {_, Id} = lists:keyfind(<<"id">>, 1, Props),
    do_put(record, [{layer, Layer}, {id, Id}], {struct, Props}).

del_record({struct, Props}) ->
    {_, Layer} = lists:keyfind(<<"layer">>, 1, Props),
    {_, Id} = lists:keyfind(<<"id">>, 1, Props),
    do_del(record, [{layer, Layer}, {id, Id}]. {struct, Props}).

get_history(Layer, Id) ->
    do_get(history, [{layer, Layer}, {id, Id}]).

get_nearby_geohash(Layer, GeoHash) ->
    do_get(nearby_geohash, [{layer, Layer}, {geohash, GeoHash}]).

get_nearby(Layer, Lat, Lon) ->
    do_get(nearby, [{layer, Layer}, {lat, Lat}, {lon, Lon}]).

get_nearby_address(Lat, Lon) ->
    do_get(nearby_address, [{lat, Lat}, {lon, Lon}]).

get_density_day(Day, Lat, Lon) ->
    do_get(density_day, [{day, Day}, {lat, Lat}, {lon, Lon}]).

get_density_hour(Hour, Lat, Lon) ->
    do_get(density_hour, [{hour, Hour}, {lat, Lat}, {lon, Lon}]).

get_layer(Layer) ->
    do_get(layer, [{layer, Layer}]).

get_contains(Lat, Lon) ->
    do_get(contains, [{lat, Lat}, {lon, Lon}]).

get_overlaps(South, West, North, East) ->
    do_get(overlaps, [{south, South},
                      {west, West},
                      {north, North},
                      {east, East}]).

get_boundary(Id) ->
    do_get(boundary, [{id, Id}]).


%% Internal

do_get(Endpoint, Params) ->
    do_request(simplegeo_oauth, get,
               [simplegeo_endpoints:endpoint(Endpoint, Params)]).

do_put(Endpoint, Params, Body) ->
    do_request(simplegeo_oauth, put,
               [simplegeo_endpoints:endpoint(Endpoint, Params), Body]).

do_del(Endpoint, Params, Body) ->
    do_request(simpleget_oauth, del,
	       [simplegeo_endpoints:endpoint(Endpoint, Params), Body]).

do_request(Module, Fun, Args) ->
    case erlang:apply(Module, Fun, Args) of
        {ok, {{Code, _}, _, Body}} when Code =:= 200 orelse
                                        Code =:= 202 ->
            {ok, mochijson2:decode(Body)};
        {ok, {{_Error, _}, _, Body}} ->
            {struct, ErrorBody} = mochijson2:decode(Body),
            {_, Message} = lists:keyfind(<<"message">>, 1, ErrorBody),
            {_, Code} = lists:keyfind(<<"code">>, 1, ErrorBody),
            {error, {Code, Message}};
        {error, Error} ->
            {error, Error}
    end.
