%%% @doc A device that finds `PATCH' requests in the `results/outbox'
%%% of its message, and applies them to it. This can be useful for processes
%%% whose computation would like to manipulate data outside of the `results' key
%%% of its message.
-module(dev_patch).
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% @doc Default process device hooks.
init(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
normalize(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
snapshot(Msg1, _Msg2, _Opts) -> {ok, Msg1}.

%% @doc Find `PATCH' requests in the `results/outbox' of the message, and apply
%% them to the state.
compute(Msg1, Msg2, Opts) ->
    % Find the input keys.
    PatchFrom = hb_converge:get_first(
        [
            {Msg2, <<"patch-from">>},
            {Msg1, <<"patch-from">>}
        ],
        <<"/results/outbox">>,
        Opts
    ),
    PatchTo = hb_converge:get_first(
        [
            {Msg2, <<"patch-to">>},
            {Msg1, <<"patch-to">>}
        ],
        <<"/">>,
        Opts
    ),
    ?event({patch_from, PatchFrom}),
    ?event({patch_to, PatchTo}),
    % Get the outbox from the message.
    Outbox = hb_converge:get(PatchFrom, Msg1, #{}, Opts),
    % Find all messages with the PATCH request.
    Patches =
        maps:filter(
            fun(_, Msg) ->
                hb_converge:get(<<"method">>, Msg, Opts) == <<"PATCH">>
            end,
            Outbox
        ),
    OutboxWithoutPatches = maps:without(maps:keys(Patches), Outbox),
    % Find the state to apply the patches to.
    % Apply the patches to the state.
    PatchedSubmessage =
        maps:fold(
            fun(_, Patch, MsgN) ->
                ?event({patching, {patch, Patch}, {before, MsgN}}),
                Res = hb_converge:set(
                    MsgN,
                    maps:without([<<"method">>], Patch),
                    Opts
                ),
                ?event({patched, {'after', Res}}),
                Res
            end,
            case PatchTo of
                not_found -> Msg1;
                PatchTo -> hb_converge:get(PatchTo, Msg1, Opts)
            end,
            Patches
        ),
    PatchedState =
        case PatchTo of
            <<"/">> -> PatchedSubmessage;
            _ -> hb_converge:set(Msg1, PatchTo, PatchedSubmessage, Opts)
        end,
    % Return the patched message and the source, less the patches.
    Res = {
        ok,
        hb_converge:set(
            PatchedState,
            PatchFrom,
            OutboxWithoutPatches,
            Opts
        )
    },
    ?event({patch_result, Res}),
    Res.

%%% Tests

uninitialized_patch_test() ->
    InitState = #{
        <<"device">> => <<"patch@1.0">>,
        <<"results">> => #{
            <<"outbox">> => #{
                <<"1">> => #{
                    <<"method">> => <<"PATCH">>,
                    <<"prices">> => #{
                        <<"apple">> => 100,
                        <<"banana">> => 200
                    }
                },
                <<"2">> => #{
                    <<"method">> => <<"GET">>,
                    <<"prices">> => #{
                        <<"apple">> => 1000
                    }
                }
            }
        },
        <<"other-message">> => <<"other-value">>,
        <<"patch-to">> => <<"/">>,
        <<"patch-from">> => <<"/results/outbox">>
    },
    {ok, ResolvedState} =
        hb_converge:resolve(
            InitState,
            <<"compute">>,
            #{}
        ),
    ?event({resolved_state, ResolvedState}),
    ?assertEqual(
        100,
        hb_converge:get(<<"prices/apple">>, ResolvedState, #{})
    ),
    ?assertMatch(
        not_found,
        hb_converge:get(<<"results/outbox/1">>, ResolvedState, #{})
    ).

patch_to_submessage_test() ->
    InitState = #{
        <<"device">> => <<"patch@1.0">>,
        <<"results">> => #{
            <<"outbox">> => #{
                <<"1">> =>
                    hb_message:attest(#{
                        <<"method">> => <<"PATCH">>,
                        <<"prices">> => #{
                            <<"apple">> => 100,
                            <<"banana">> => 200
                        }
                    },
                    hb:wallet()
                )
            }
        },
        <<"state">> => #{
            <<"prices">> => #{
                <<"apple">> => 1000
            }
        },
        <<"other-message">> => <<"other-value">>,
        <<"patch-to">> => <<"/state">>,
        <<"patch-from">> => <<"/results/outbox">>
    },
    {ok, ResolvedState} =
        hb_converge:resolve(
            InitState,
            <<"compute">>,
            #{}
        ),
    ?event({resolved_state, ResolvedState}),
    ?assertEqual(
        100,
        hb_converge:get(<<"state/prices/apple">>, ResolvedState, #{})
    ).