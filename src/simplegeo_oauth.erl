-module(simplegeo_oauth).

-export([get/1, get/2]).
-export([put/2, put/3]).

consumer() ->
    {ok, ConsumerKey} = application:get_env(simplegeo, consumer_key),
    {ok, ConsumerSecret} = application:get_env(simplegeo, consumer_secret),
    {ok, SignatureMethod} = application:get_env(simplegeo, signature_method),

    {ConsumerKey, ConsumerSecret, SignatureMethod}.

get(Endpoint) ->
    get(Endpoint, []).

get(Endpoint, Data) ->
    get(consumer(), Endpoint, Data).

get(Consumer, Endpoint0, Data) ->
    Endpoint = case Data of
                   [] ->
                       Endpoint0;
                   [{_, _} | _] ->
                       Endpoint0 ++ "?" ++ mochiweb_util:urlencode(Data)
               end,

    request(Consumer, "GET", Endpoint, []).


put(Endpoint, Data) ->
    put(consumer(), Endpoint, Data).

put(Consumer, Endpoint, Data) ->
    Body = case Data of
               [{_, _} | _] ->
                   mochiweb_util:urlencode(Data);
               {struct, _} ->
                   iolist_to_binary(mochijson2:encode(Data));
               _ ->
                   Data
           end,

    request(Consumer, "PUT", Endpoint, Body).


request(Consumer, Method, Endpoint, Body) ->
    SignedParams = oauth:signed_params(Method,
                                       Endpoint, [], Consumer,
                                       "", ""),
    lhttpc:request(oauth:uri(Endpoint, SignedParams),
                   Method,
                   [],
                   Body,
                   10000,
                   []).
