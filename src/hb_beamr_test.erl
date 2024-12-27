-module(hb_beamr_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-hb_debug(print).

%% WASM files
-define(WASM_CORE, "test/aot/core.aot").
% -define(WASM_SQL, "test/interp/sqlite.wasm").

%% Helper function to parse the WASM response
parse_wasm_response(ResponseBin) ->
    case jiffy:decode(ResponseBin, [return_maps]) of
        #{<<"ok">> := true, <<"response">> := #{<<"Output">> := #{<<"data">> := Data}}} ->
            {ok, Data};
        Other ->
            {error, {unexpected_response_structure, Other}}
    end.


% %% Test performance of the `handle` function
performance_test() ->
    WasmFile = ?WASM_CORE,
    WasmBinary = setup_env(WasmFile),
    ?event("Running aos64_performance_test"),
    {ok, Port, _Imports, _Exports} = hb_beamr:start(WasmBinary),
    Env = gen_test_env(),
    Msg = gen_test_aos_msg(
        "local sum = 0; " ++
        "for i = 1, 100000 do " ++
        "    sum = sum + i; " ++
        "end; " ++
        "sum"
    ),

    %% Write data to WASM memory
    {ok, EnvPtr} = hb_beamr_io:write_string(Port, Env),
    {ok, MsgPtr} = hb_beamr_io:write_string(Port, Msg),

    %% Time the execution of the "handle" function
    {Time, _Res} = timer:tc(
        fun() ->
            {ok, [ResultPtr]} = hb_beamr:call(Port, "handle", [MsgPtr, EnvPtr]),
            {ok, ResponseBin} = hb_beamr_io:read_string(Port, ResultPtr),
            ?event(io_lib:format("ResponseBin: ~p", [ResponseBin])),
            case parse_wasm_response(ResponseBin) of
                {ok, Response} ->
					?event(io_lib:format("Response: ~p", [Response])),
					?assertMatch(<<"5000050000">>, Response);
                {error, Reason} ->
                    ?event({"Unexpected response during performance test", Reason}),
                    ?assert(false)  % Fail the test explicitly
            end
        end
    ),
    ?event(io_lib:format("Execution time: ~p microseconds", [Time])),
    ?assert(Time < 10000000),  % Ensure execution time is less than 10 seconds

    %% Stop the WASM
    hb_beamr:stop(Port),
    ?event("aos64_performance_test passed").

% % Test the `handle` function for correctness
% eval_test() ->
%     WasmFile = ?WASM_CORE,
%     WasmBinary = setup_env(WasmFile),
%     ?event("Running aos64_handle_test"),
%     {ok, Port, _Imports, _Exports} = hb_beamr:start(WasmBinary),
%     Env = gen_test_env(),
%     Msg = gen_test_aos_msg("1+5"),

%     %% Write data to WASM memory
%     {ok, EnvPtr} = hb_beamr_io:write_string(Port, Env),
%     {ok, MsgPtr} = hb_beamr_io:write_string(Port, Msg),

%     %% Call the "handle" function in the WASM
%     {ok, [ResultPtr]} = hb_beamr:call(Port, "handle", [MsgPtr, EnvPtr]),

%     %% Read and parse the result
%     {ok, ResponseBin} = hb_beamr_io:read_string(Port, ResultPtr),
%     case parse_wasm_response(ResponseBin) of
%         {ok, Response} ->
% 			?event(io_lib:format("Response: ~p", [Response])),
% 			?assertMatch(<<"6">>, Response);
%         {error, Reason} ->
%             ?event({"Unexpected response", Reason}),
%             ?assert(false)  % Fail the test explicitly
%     end,

%     %% Stop the WASM
%     hb_beamr:stop(Port),
%     ?event("aos64_handle_test passed").

%% Test the `handle` function for correctness
% eval_test() ->
%     WasmFile = ?WASM_CORE,
%     WasmBinary = setup_env(WasmFile),
%     ?event("Running aos64_handle_test"),
%     {ok, Port, _Imports, _Exports} = hb_beamr:start(WasmBinary),
%     Env = gen_test_env(),
%     Msg = gen_test_aos_msg("return 1+5"),

%     %% Write data to WASM memory
%     {ok, EnvPtr} = hb_beamr_io:write_string(Port, Env),
%     {ok, MsgPtr} = hb_beamr_io:write_string(Port, Msg),

%     %% Call the "handle" function in the WASM
%     {ok, [ResultPtr]} = hb_beamr:call(Port, "handle", [MsgPtr, EnvPtr]),

%     %% Read and parse the result
%     {ok, ResponseBin} = hb_beamr_io:read_string(Port, ResultPtr),
%     case parse_wasm_response(ResponseBin) of
%         {ok, Response} ->
% 			?event(io_lib:format("Response: ~p", [Response])),
% 			?assertMatch(<<"5">>, Response);
%         {error, Reason} ->
%             ?event({"Unexpected response", Reason}),
%             ?assert(false)  % Fail the test explicitly
%     end,

%     %% Stop the WASM
%     hb_beamr:stop(Port),
%     ?event("aos64_handle_test passed").



%% Test performance of the `handle` function
% aos64_sql_test() ->
%     WasmFile = ?WASM_SQL,
%     WasmBinary = setup_env(WasmFile),
%     ?event("Running aos64_performance_test"),
%     {ok, Port, _Imports, _Exports} = hb_beamr:start(WasmBinary),
%     Env = gen_test_env(),
% 	Msg = gen_test_aos_msg(
% 		"local json = require('json'); " ++
% 		"local sqlite = require('lsqlite3'); " ++
% 		"local db = sqlite.open(':memory:'); " ++
% 		"db:exec('CREATE TABLE numbers(num)'); " ++
% 		"for i=1,2 do " ++
% 		"    db:exec('INSERT INTO numbers VALUES ('..i..')'); " ++
% 		"end; " ++
% 		"t = {}; " ++
% 		"for a in db:nrows('select * from numbers') do table.insert(t, a) end; " ++
% 		"db:close(); " ++
% 		"return json.encode(t);"
% 	),

%     %% Write data to WASM memory
% 	{ok, EnvPtr} = hb_beamr_io:write_string(Port, Env),
% 	{ok, MsgPtr} = hb_beamr_io:write_string(Port, Msg),

% 	%% Call the "handle" function in the WASM
% 	{ok, [ResultPtr]} = hb_beamr:call(Port, "handle", [MsgPtr, EnvPtr]),

% 	%% Read and parse the result
% 	{ok, ResponseBin} = hb_beamr_io:read_string(Port, ResultPtr),
% 	case parse_wasm_response(ResponseBin) of
% 		{ok, Response} ->
% 			?event(io_lib:format("Response: ~p", [Response])),
% 			?assertMatch(<<"[{\"num\":1},{\"num\":2}]">>, Response);
% 		{error, Reason} ->
% 			?event({"Unexpected response", Reason}),
% 			?assert(false)  % Fail the test explicitly
% 	end,

% 	%% Stop the WASM
% 	hb_beamr:stop(Port),
% 	?event("aos64_sql_test passed").


%% Helper: Set up environment and read WASM file
setup_env(WasmFile) ->
    %% Ensure the WASM file exists and can be read
    ?event(io_lib:format("Setting up test environment with ~ts", [WasmFile])),
    case file:read_file(WasmFile) of
        {ok, WasmBinary} ->
            WasmBinary;
        {error, Reason} ->
            ?event(io_lib:format("Failed to read WASM file: ~ts, Reason: ~p", [WasmFile, Reason])),
            throw({error, {wasm_file_read_failed, WasmFile, Reason}})
    end.

%% Generate a test environment JSON string
gen_test_env() ->
    Json =
        "{
            \"Process\": {
                \"Id\": \"AOS\",
                \"Owner\": \"FOOBAR\",
                \"Tags\": [
                    {\"name\": \"Name\", \"value\": \"Thomas\"},
                    {\"name\": \"Authority\", \"value\": \"FOOBAR\"}
                ]
            }
        }",
    <<(list_to_binary(Json))/binary, 0>>.

%% Generate a test AOS message JSON string with a custom command
gen_test_aos_msg(Command) ->
    Json =
        "{
            \"From\": \"FOOBAR\",
            \"Block-Height\": \"1\",
            \"Target\": \"AOS\",
            \"Owner\": \"FOOBAR\",
            \"Id\": \"1\",
            \"Module\": \"W\",
            \"Tags\": [
                {\"name\": \"Action\", \"value\": \"Eval\"}
            ],
            \"Data\": \"" ++ Command ++ "\"
        }",
    <<(list_to_binary(Json))/binary, 0>>.
