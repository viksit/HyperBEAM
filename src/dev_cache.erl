%%% @doc A device that looks up an ID from a local store and returns it, honoring
%%% the `accept' key to return the correct format. The cache also supports
%%% writing messages to the store, if the node message has the writer's address
%%% in its `cache_writers' key.
-module(dev_cache).
-export([read/3, write/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

read(_M1, M2, Opts) ->
    ID = hb_converge:get(<<"target">>, M2, Opts),
    ?event({lookup, {id, ID}, {opts, Opts}}),
    case hb_cache:read(ID, Opts) of
        {ok, Res} ->
            ?event({lookup_result, Res}),
            case hb_converge:get(<<"accept">>, M2, Opts) of
                <<"application/aos-2">> ->
                    Struct = dev_json_iface:message_to_json_struct(Res),
                    {ok,
                        #{
                            <<"body">> => jiffy:encode(Struct),
                            <<"content-type">> => <<"application/aos-2">>
                        }};
                _ ->
                    {ok, Res}
            end;
        not_found ->
            ?event({lookup_not_found, ID}),
            {error, not_found}
    end.

write(_M1, M2, Opts) ->
    case is_trusted_writer(M2, Opts) of
        true ->
            Type = hb_converge:get(<<"type">>, M2, <<"single">>, Opts),
            case Type of
                <<"single">> -> write_single(M2, Opts);
                <<"batch">> ->
                    maps:map(
                        fun(_, Value) ->
                            write_single(Value, Opts)
                        end,
                        hb_converge:get(<<"body">>, M2, Opts)
                    );
                _ ->
                    {error,
                        #{
                            <<"status">> => 400,
                            <<"body">> => <<"Invalid write type.">>
                        }
                    }
            end;
        false ->
            {error, 
                #{
                    <<"status">> => 403,
                    <<"body">> => <<"Not authorized to write to the cache.">>
                }
            }
    end.

write_single(Msg, Opts) ->
    Body = hb_converge:get(<<"body">>, Msg, Opts),
    Location = hb_converge:get(<<"location">>, Msg, Opts),
    Operation = hb_converge:get(<<"operation">>, Msg, <<"write">>, Opts),
    case {Operation, Body, Location} of
        {<<"write">>, not_found, _} ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"No body to write.">>
                }
            };
        {<<"write">>, Map, _Location} when is_map(Map) ->
            ok = hb_cache:write(Map, Opts),
            {ok, #{ <<"status">> => 200 }};
        {<<"write">>, Binary, Location} when is_binary(Binary) ->
            ok = hb_cache:write_binary(Location, Binary, Opts),
            {ok, #{ <<"status">> => 200 }};
        {<<"link">>, _, _} ->
            Source = hb_converge:get(<<"source">>, Msg, Opts),
            Destination = hb_converge:get(<<"destination">>, Msg, Opts),
            ok = hb_cache:link(Source, Destination, Opts),
            {ok, #{ <<"status">> => 200 }};
        _ ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Invalid write type.">>
                }
            }
    end.

is_trusted_writer(Req, Opts) ->
    case hb_message:signers(Req) of
        [Signer] ->
            lists:member(Signer, hb_opts:get(cache_writers, [], Opts));
        _ -> false
    end.

%%% Tests

binary_lookup_test() ->
    Bin = <<"Simple unsigned data item">>,
    {ok, ID} = hb_cache:write(Bin, #{}),
    {ok, RetrievedBin} = read(#{}, #{ <<"target">> => ID }, #{}),
    ?assertEqual(Bin, RetrievedBin).

message_lookup_test() ->
    Msg = #{ <<"test-key">> => <<"test-value">>, <<"data">> => <<"test-data">> },
    {ok, ID} = hb_cache:write(Msg, #{}),
    {ok, RetrievedMsg} = read(#{}, #{ <<"target">> => ID }, #{}),
    ?assertEqual(Msg, RetrievedMsg).

aos2_message_lookup_test() ->
    Msg = #{ <<"test-key">> => <<"test-value">>, <<"data">> => <<"test-data">> },
    {ok, ID} = hb_cache:write(Msg, #{}),
    {ok, RetrievedMsg} =
        read(
            #{},
            #{ <<"target">> => ID, <<"accept">> => <<"application/aos-2">> },
            #{}
        ),
    Decoded = jiffy:decode(hb_converge:get(<<"body">>, RetrievedMsg, #{}), [return_maps]),
    ?assertEqual(<<"test-data">>, hb_converge:get(<<"data">>, Decoded, #{})).

http_lookup_test() ->
    Store = {
        hb_store_fs,
        #{ prefix => "mainnet-cache" }
    },
    Opts = #{ store => Store },
    Msg = #{ <<"test-key">> => <<"test-value">>, <<"data">> => <<"test-data">> },
    {ok, ID} = hb_cache:write(Msg, Opts),
    Node = hb_http_server:start_node(Opts),
    Wallet = hb:wallet(),
    Req = hb_message:attest(#{
        <<"path">> => <<"/~cache@1.0/read?target=", ID/binary>>,
        <<"device">> => <<"cache@1.0">>,
        <<"accept">> => <<"application/aos-2">>
    }, Wallet),
    {ok, Res} = hb_http:post(Node, Req, Opts),
    Decoded = jiffy:decode(hb_converge:get(<<"body">>, Res, Opts), [return_maps]),
    ?assertEqual(<<"test-data">>, hb_converge:get(<<"data">>, Decoded, Opts)).