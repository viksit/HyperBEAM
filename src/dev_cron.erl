-module(dev_cron).
-export([init/2, execute/2, uses/0, find_block_at_time/3]).

%%% A device that inserts new messages into the schedule to allow processes
%%% to passively 'call' themselves without user interaction.

-include("include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CRON_INTERVAL, <<"Cron-Interval">>).

-type optional_non_neg_integer() :: non_neg_integer() | undefined.

-record(schedule, {
	name :: binary(),
	unit :: binary(),
	scalar :: non_neg_integer()
}).

-record(state, {
	% For each corresponding Cron Schedule, The timestamp and block
	% that should trigger the next Cron message on that schedule.
	%
	% Notice that all timestamps are optional, as it won't be immediately
	% available in some cases
	nexts :: [{optional_non_neg_integer(), {optional_non_neg_integer(), non_neg_integer()}}],
	% An array of tuples, each tuple containing a cron schedule, and its associated tags
	crons :: [{#schedule{}, []}]
}).

init(State = #{process := Process}, _Params) ->
	case parse_crons(Process#tx.tags) of
		[] ->
			{ok, State#{cron => inactive}};
		Crons ->
			{ProcessTimestamp, ProcessBlockHeight} = block(Process),
			CronState = maps:get("cron", State, #state{}),
			% Re-hydrate the state of the device, extracting 'next' values
			% from the provided state, or calculating next for each cron w.r.t the process origin
			CronNexts =
				CronState#state.crons =/= undefined andalso
					CronState#state.nexts orelse
					lists:map(
						fun(Cron) ->
							case Cron#schedule.unit of
								<<"block">> -> ProcessBlockHeight + Cron#schedule.scalar;
								<<"millisecond">> -> ProcessTimestamp + Cron#schedule.scalar
							end
						end,
						Crons
					),
			?no_prod("How will the cron state be initialized on State? Presumably from a checkpoint?"),
			{
				ok,
				State#{
					cron => #state{
						nexts = CronNexts,
						crons = Crons
					}
				}
			}
	end.

execute(_Msg, State = #{cron := inactive}) ->
	{ok, State};
execute(Msg, State) ->
	{NewNexts, NewSched} = maybe_append_cron_msgs(State, block(Msg)),
	NewCron = maps:put("nexts", NewNexts, maps:get("cron", State)),
	{ok, State#{schedule := NewSched, cron := NewCron}}.

uses() -> all.

%%% ================================
%%% Private Functions
%%% ================================

maybe_append_cron_msgs(
	#{
		process := Process,
		schedule := Sched,
		cron := #state{
			nexts = Nexts,
			crons = Crons
		}
	},
	Right
) ->
	CronsWithNexts = lists:zip(Crons, Nexts, fail),
	{NewNexts, NewMsgs} = maybe_append_cron_msgs([], [], CronsWithNexts, Process, Right),
	{NewNexts, NewMsgs ++ Sched}.

maybe_append_cron_msgs(NewNexts, NewMsgs, [], _Process, _Right) ->
	{
		NewNexts,
		% Sort any newly generated cron messages by timestamp ascending
		% Such that they are prepended to the schedule in order
		lists:sort(
			fun(Msg1, Msg2) ->
				Timestamp1 = binary_to_integer(ar_util:find_value(Msg1#tx.tags, <<"Timestamp">>)),
				Timestamp2 = binary_to_integer(ar_util:find_value(Msg2#tx.tags, <<"Timestamp">>)),
				Timestamp1 =< Timestamp2
			end,
			NewMsgs
		)
	};
maybe_append_cron_msgs(
	NewNexts,
	NewMsgs,
	[
		{
			Cron = {Schedule, _Tags},
			Next = {NextTime, {NextBlockTime, NextHeight}}
		}
		| Rest
	],
	Process,
	Right = {RightTime, RightHeight}
) ->
	?no_prod(
		"How should we handle Cron messages on the 'edge' of an eval stream ie. 'latest' & empty schedule, and so no Right? Initialize to 'current'?"
	),
	{NewNext, NNewMsgs} =
		case Schedule#schedule.unit of
			<<"block">> ->
				if
					NextHeight < RightHeight ->
						{Timestamp, _} = block(NextHeight),
						% The block that will trigger the next block-based Cron message
						% may not yet be created, and so we may not be able to determine the
						% corresponding timestamp, right now.
						%
						% So we leave the timestamps as undefined in 'next', to be determined when the next block-based Cron message
						% is inserted into the schedule (see line above).
						NNext = {undefined, {undefined, NextHeight + Schedule#schedule.scalar}},
						{NNext, [create_cron_msg(Process, Cron, Timestamp * 1000, NextHeight) | NewMsgs]};
					% Do nothing, adding no new Msg and keeping the current Next as is
					true ->
						{Next, NewMsgs}
				end;
			<<"millisecond">> ->
				if
					NextTime < RightTime ->
						% Arweave blocks are created every ~120 seconds, so we can use this as a heuristic
						% to determine whether or not we need to fetch new block metadata to use as part
						% of the generated Cron message.
						%
						% If the Cron message being generated is within 90 seconds of the current block
						% then we shouldn't need to search arweave for the block at the 'NextTime' --
						% the block should still just be the current block.
						%
						% This is a boon for any cron-schedule that fits within a single block,
						% especially for rapid, sub-second schedules, as we do not try to repeatedly
						% fetch the same block from arweave
						%
						% TODO: perhaps this optimization can be pushed into a lower layer ie.
						% arweave_timestamp() caching block metadata instead. Then this check
						% could simply be removed
						{CurBlockTime, CurHeight} = {NextBlockTime, NextHeight},
						{BlockTimestamp, Height} =
							case NextTime - CurBlockTime of
								N when N =< 90000 ->
									{CurBlockTime, CurHeight};
								_ ->
									% The NextTime doesn't fit within our heuristic, so search
									% for the block on Arweave, using the CurHeight and RightHeight to constrain
									% The search space, then compare that blocks time to the NextTime
									{InSeconds, H} = find_block_at_time(CurHeight, RightHeight, NextTime / 1000),
									% Make sure to use convert the block time into milliseconds
									(NextTime < (InSeconds * 1000)) andalso
										{CurBlockTime, CurHeight} orelse
										{InSeconds * 1000, H}
							end,
						NNext = {NextTime + Schedule#schedule.scalar, {BlockTimestamp, Height}},
						{NNext, [create_cron_msg(Process, Cron, NextTime * 1000, Height) | NewMsgs]};
					% Do nothing, adding no new Msg and keeping the current Next as is
					true ->
						{Next, NewMsgs}
				end
		end,
	maybe_append_cron_msgs(NewNexts ++ [NewNext], NNewMsgs, Rest, Process, Right).

create_cron_msg(Process, {_, CronTags}, Timestamp, BlockHeight) ->
	?no_prod("What about derived top-lvl like Timestamp, Block-Height, From, Cron, etc.?"),
	#tx{
		owner = Process#tx.owner,
		target = Process#tx.id,
		tags =
			[
				{<<"Data-Protocol">>, <<"ao">>},
				{<<"Variant">>, <<"ao.TN.2">>},
				{<<"Process">>, Process#tx.id},
				{<<"Target">>, Process#tx.id},
				{<<"Timestamp">>, list_to_binary(integer_to_list(Timestamp))},
				{<<"Block-Height">>, list_to_binary(integer_to_list(BlockHeight))}
			] ++ CronTags
	}.

%%% @doc Given a Time, find the block that was the "current" block on arweave,
%%% at the given time, IN SECONDS.
%%%
%%% It is STRONGLY recommended to utilize a Low and High height
%%% to help constrain the search space. This impl employs a binary search, querying
%%% arweave for block metadata.
%%%
%%% TODO: maybe should be abstracted behind ao_client:arweave_timestamp({timestamp, Time}, Low, High)
find_block_at_time(LowHeight, HighHeight, Time) -> find_block_at_time(LowHeight, HighHeight, Time, LowHeight).
find_block_at_time(LowHeight, HighHeight, _Time, CurHeight) when LowHeight > HighHeight ->
	block(CurHeight);
find_block_at_time(LowHeight, HighHeight, Time, CurHeight) ->
	Mid = LowHeight + (HighHeight - LowHeight) div 2,
	{CurTime, _} = block(Mid),
	if
		CurTime =< Time -> find_block_at_time(Mid + 1, HighHeight, Time, Mid);
		true -> find_block_at_time(LowHeight, Mid - 1, Time, CurHeight)
	end.

block(Tx) when is_record(Tx, tx) ->
	?no_prod("Is a timestamp tag seconds or milliseconds?"),
	{
		binary_to_integer(ar_util:find_value(Tx#tx.tags, <<"Timestamp">>)),
		binary_to_integer(ar_util:find_value(Tx#tx.tags, <<"Block-Height">>))
	};
block(Height) when is_integer(Height) ->
	{Timestamp, Height, _} = ao_client:arweave_timestamp(Height),
	% Arweave timestamps are in seconds, so convert to milliseconds
	{Timestamp, Height}.

%%%
%%% Parse a set of ao tags into a list of Cron Schedules
%%%
parse_crons(Tags) -> parse_crons([], Tags).

parse_crons(Crons, []) ->
	lists:reverse(Crons);
parse_crons(Crons, [{?CRON_INTERVAL, Value} | RestTags]) ->
	parse_crons([{parse_interval(Value), []} | Crons], RestTags);
parse_crons([], [_ | RestTags]) ->
	% Cron-Tag-* are associated with the most-recent Cron-Interval
	% so if there are no Intervals yet found, then there is nothing to do
	parse_crons([], RestTags);
parse_crons(Crons, [Tag | RestTags]) ->
	parse_crons(maybe_append_cron_tag(Crons, Tag), RestTags).

maybe_append_cron_tag(Crons, {Name, Value}) ->
	Parsed = binary_to_list(Name),
	case Parsed of
		"Cron-Tag-" ++ _ ->
			[{Interval, CronTags} | Rest] = Crons,
			TagName = list_to_binary(string:slice(Parsed, length("Cron-Tag-"))),
			[{Interval, CronTags ++ [{TagName, Value}]} | Rest];
		_ ->
			Crons
	end.

parse_interval(Interval) ->
	[AmountStr, UnitStr] = lists:map(
		fun(S) -> string:trim(S) end,
		string:split(Interval, <<"-">>)
	),
	Amount = binary_to_integer(AmountStr),
	Unit = string:lowercase(binary_to_list(UnitStr)),
	case Unit of
		"millisecond" ++ _ -> to_time_schedule(Interval, Amount);
		"second" ++ _ -> to_time_schedule(Interval, Amount * 1000);
		"minute" ++ _ -> to_time_schedule(Interval, Amount * 60 * 1000);
		"hour" ++ _ -> to_time_schedule(Interval, Amount * 60 * 60 * 1000);
		"day" ++ _ -> to_time_schedule(Interval, Amount * 24 * 60 * 60 * 1000);
		"week" ++ _ -> to_time_schedule(Interval, Amount * 7 * 24 * 60 * 60 * 1000);
		"year" ++ _ -> to_time_schedule(Interval, Amount * 365 * 24 * 60 * 60 * 1000);
		"block" ++ _ -> to_block_schedule(Interval, Amount);
		_ -> throw({error, invalid_cron_interval, UnitStr})
	end.

to_time_schedule(Name, Millis) -> #schedule{name = Name, unit = <<"millisecond">>, scalar = Millis}.
to_block_schedule(Name, Blocks) -> #schedule{name = Name, unit = <<"block">>, scalar = Blocks}.

%%%
%%% TESTS
%%%

find_block_at_time_test() ->
	TargetBlock = {1731090175, 1543761},
	{TargetTime, _} = TargetBlock,

	ExactTime = find_block_at_time(1543732, 1543773, TargetTime),
	?assertEqual(TargetBlock, ExactTime),
	AfterTime = find_block_at_time(1543722, 1543773, TargetTime + 100),
	?assertEqual(TargetBlock, AfterTime).

parse_crons_test() ->
	Tags = [
		{<<"Foo">>, <<"Bar">>},
		{?CRON_INTERVAL, <<"10-blocks">>},
		{<<"Cron-Tag-Action">>, <<"notify">>},
		{<<"Cron-Tag-Action-Function">>, <<"transfer">>},
		{<<"Random">>, <<"Tag">>},
		{?CRON_INTERVAL, <<" 10-minutes ">>},
		{<<"Cron-Tag-Action">>, <<"notify">>},
		{<<"Cron-Tag-Action-Function">>, <<"transfer">>},
		{?CRON_INTERVAL, <<"1-hour">>},
		{<<"Another">>, <<"Tag">>},
		{<<"Cron-Tag-Action">>, <<"transfer">>}
	],
	[{BSchedule, BTags}, {MSchedule, MTags}, {HSchedule, HTags}] = parse_crons(Tags),

	?assertEqual(#schedule{name = <<"10-blocks">>, unit = <<"block">>, scalar = 10}, BSchedule),
	?assertEqual([{<<"Action">>, <<"notify">>}, {<<"Action-Function">>, <<"transfer">>}], BTags),

	?assertEqual(#schedule{name = <<" 10-minutes ">>, unit = <<"millisecond">>, scalar = 10 * 60 * 1000}, MSchedule),
	?assertEqual([{<<"Action">>, <<"notify">>}, {<<"Action-Function">>, <<"transfer">>}], MTags),

	?assertEqual(#schedule{name = <<"1-hour">>, unit = <<"millisecond">>, scalar = 60 * 60 * 1000}, HSchedule),
	?assertEqual([{<<"Action">>, <<"transfer">>}], HTags).

maybe_append_cron_msgs_test() ->
	OneMinute = 1000 * 60,
	ProcessTime = 1719117985000 + 1000,
	ProcessHeight = 1450536,
	Process = #tx{
		id = "process-123",
		owner = "owner-123",
		tags = [
			{<<"Timestamp">>, list_to_binary(integer_to_list(ProcessTime))},
			{<<"Block-Height">>, list_to_binary(integer_to_list(ProcessHeight))}
		]
	},
	Schedule = [#tx{id = "some-message"}],
	Crons = [
		{#schedule{name = "10-minutes", unit = <<"millisecond">>, scalar = OneMinute * 10}, [{<<"Foo">>, <<"Bar">>}]},
		{#schedule{name = "2-blocks", unit = <<"block">>, scalar = 2}, [{<<"Bar">>, <<"Bazz">>}]},
		{#schedule{name = "15-minutes", unit = <<"millisecond">>, scalar = OneMinute * 15}, [{<<"Another">>, <<"One">>}]},
		{#schedule{name = "2-blocks", unit = <<"block">>, scalar = 2}, [{<<"Final">>, <<"Buzz">>}]}
	],
	Nexts = [
		{ProcessTime + OneMinute * 10, {1719118591000, 1450542}},
		{undefined, {undefined, 1450538}},
		{ProcessTime + OneMinute * 15, {1719118711000, 1450544}},
		{undefined, {undefined, 1450538}}
	],
	State = #{
		process => Process,
		schedule => Schedule,
		cron => #state{
			nexts = Nexts,
			crons = Crons
		}
	},

	% should not increment nexts or add new messages, when the schedule has not yet arrived at the crons
	{NoNewNexts, NoNewMsgs} = maybe_append_cron_msgs(State, {ProcessTime + 10000, ProcessHeight}),
	?assertEqual(Nexts, NoNewNexts),
	?assertEqual(Schedule, NoNewMsgs).

% TODO add the other test cases ie. partial new nexts, all new nexts
