%%% @doc A simple ledger implementing the p4@1.0 interface, using a process@1.0
%%% as its source-of-truth. Ignores requests to credit the ledger, instead 
%%% allowing the process to manage balances itself.
-module(dev_ledger_process).
-export([balance/3, debit/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

balance(LedgerMsg, RawReq, NodeMsg) ->
    Target =
        case hb_converge:get(<<"request">>, RawReq, NodeMsg#{ hashpath => ignore }) of
            not_found -> hd(hb_message:signers(RawReq));
            Req -> hd(hb_message:signers(Req))
        end,
    Results = 
        ledger_call(
            LedgerMsg,
            #{ <<"action">> => <<"balance">>, <<"address">> => Target },
            NodeMsg
        ),
    Balance =
        try
            {
                ok,
                binary_to_integer(
                    hb_converge:get(<<"data">>, Results, #{})
                )
            }
        catch error:badarg ->
            {error, Results#{ <<"error">> => <<"bad_ledger">> }}
        end,
    {ok, Balance}.

debit(_, Req, NodeMsg) ->
    {ok, true}.

%% @doc Get the result of a message on the ledger process.
ledger_call(LedgerMsg, Req, NodeMsg) ->
    Process = hb_converge:get(<<"process">>, LedgerMsg, NodeMsg),
    ProcessID = hb_converge:get(<<"id">>, Process, NodeMsg),
    SignedReq = hb_message:attest(Req#{ <<"target">> => ProcessID }, NodeMsg),
    ScheduleMsg = hb_message:attest(#{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => SignedReq
    }, NodeMsg),
    {ok, #{ <<"slot">> := Slot }} = hb_converge:resolve(Process, ScheduleMsg, #{}),
    {ok, Resp} =
        hb_converge:resolve(
            Process,
            #{ <<"path">> => <<"compute">>, <<"slot">> => Slot },
            #{ spawn_worker => true }
        ),
    ?event({ledger_call, {resp, Resp}}),
    hb_converge:get(<<"results">>, Resp, #{}).

%%% Tests

setup() ->
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Process = dev_process:test_aos_process(),
    hb_cache:write(Process, #{}),
    {ok, LedgerScript} = file:read_file("aos/ledger.lua"),
    dev_process:schedule_aos_call(Process, LedgerScript),
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-message">> =>
                #{
                    <<"device">> => <<"ledger-process@1.0">>,
                    <<"process">> => Process
                },
            <<"pricing-device">> => <<"simple-pay@1.0">>
        },
    {
        Address,
        Wallet,
        Process,
        #{
            operator => Address,
            priv_wallet => Wallet,
            preprocessor => ProcessorMsg,
            postprocessor => ProcessorMsg
        }
    }.

balance_test() ->
    {_Address, Wallet, _Process, Opts} = setup(),
    Node = hb_http_server:start_node(Opts),
    Res =
        hb_http:get(
            Node,
            hb_message:attest(
                #{ <<"path">> => <<"/~p4@1.0/balance">> },
                Wallet
            ),
            #{}
        ),
    ?assertMatch({ok, #{ <<"body">> := 0 }}, Res).
