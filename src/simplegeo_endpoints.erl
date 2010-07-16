-module(simplegeo_endpoints).

-export([endpoint/2]).

endpoint(Name, Params) ->
    {ok, BaseUrl} = application:get_env(simplegeo, api_base),
    {ok, ApiVersion} = application:get_env(simplegeo, api_version),

    mochifmt:f("{base}/{version}/" ++ endpoint_pattern(Name),
               [{base, BaseUrl}, {version, ApiVersion} | Params]).


endpoint_pattern(record) ->
    "records/{layer}/{id}.json";
endpoint_pattern(records) ->
    "records/{layer}/{ids}.json";
endpoint_pattern(add_records) ->
    "records/{layer}.json";
endpoint_pattern(history) ->
    "records/{layer}/{id}/history.json";
endpoint_pattern(nearby) ->
    "records/{layer}/nearby/{lat},{lon}.json";
endpoint_pattern(nearby_geohash) ->
    "records/{layer}/nearby/{geohash}.json";
endpoint_pattern(nearby_address) ->
    "nearby/address/{lat},{lon}.json";
endpoint_pattern(density_day) ->
    "density/{day}/{lat},{lon}.json";
endpoint_pattern(density_hour) ->
    "density/{day}/{hour}/{lat},{lon}.json";
endpoint_pattern(layer) ->
    "layer/{layer}.json";
endpoint_pattern(contains) ->
    "contains/{lat},{lon}.json";
endpoint_pattern(overlaps) ->
    "overlaps/{south},{west},{north},{east}.json";
endpoint_pattern(boundary) ->
    "boundary/{id}.json".
