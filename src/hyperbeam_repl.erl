%%%-------------------------------------------------------------------
%%% @doc HyperBEAM REPL - Simple HTTP request client for HyperBEAM
%%% @end
%%%-------------------------------------------------------------------
-module(hyperbeam_repl).

-export([start/0, start/1, handle_input/1, format_response/1]).

-include("include/hb.hrl").

%% @doc Starts the REPL with default settings
start() ->
    start(#{}).

%% @doc Starts the REPL with custom options
start(Options) ->
    % Ensure HTTP client is started
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    application:ensure_all_started(hb),
    hb_http:start(),
    
    % Initialize with default configuration if not provided
    Config = maps:merge(
        #{
            host => <<"localhost">>,
            port => 10000,
            history => []
        },
        Options
    ),
    
    % Display welcome message
    print_welcome(Config),
    
    % Start the REPL loop
    repl_loop(Config).

%% @doc Main REPL loop
repl_loop(Config) ->
    % Display prompt and get input
    Input = io:get_line("\e[34mBEAMWeaver>\e[0m "),
    
    % Handle the input (removing newline)
    TrimmedInput = string:trim(Input),
    
    % Process the command
    case TrimmedInput of
        "exit" ->
            io:format("\e[33mExiting HyperBEAM REPL.\e[0m~n"),
            ok;
        "help" ->
            print_help(),
            repl_loop(Config);
        "clear" ->
            io:format("\e[2J\e[H"),  % Clear screen and move cursor to top
            print_welcome(Config),
            repl_loop(Config);
        "history" ->
            print_history(Config),
            repl_loop(Config);
        _ ->
            % Process the input as an HTTP request
            NewConfig = handle_input(TrimmedInput, Config),
            repl_loop(NewConfig)
    end.

%% @doc Handle a raw input line, processing it as an HTTP request
handle_input(Input, Config) ->
    try
        case parse_http_request(Input) of
            {Method, Path, Headers, Body} ->
                % Build URL based on config
                Host = maps:get(host, Config),
                Port = maps:get(port, Config),
                BaseURL = list_to_binary(io_lib:format("http://~s:~p", [Host, Port])),
                
                % Execute request
                Result = execute_request(Method, BaseURL, Path, Headers, Body),
                
                % Format and display response
                io:format("~s~n", [format_response(Result)]),
                
                % Update history
                History = maps:get(history, Config, []),
                Config#{history => [Input | lists:sublist(History, 19)]}; % Keep last 20 items
            invalid_request ->
                io:format("\e[31mInvalid request format. Try something like: GET /path or POST /path {\"key\":\"value\"}\e[0m~n"),
                Config
        end
    catch
        Type:Error:Stack ->
            io:format("\e[31mError processing request: ~p:~p~n~p\e[0m~n", [Type, Error, Stack]),
            Config
    end.

%% @doc Parse input line into HTTP request components
parse_http_request(Input) ->
    % Try to match different request formats
    case re:run(Input, "^(GET|POST|PUT|DELETE)\\s+(.+?)(?:\\s+(.+))?$", [{capture, all_but_first, list}]) of
        {match, [Method, Path]} ->
            {list_to_binary(Method), list_to_binary(Path), [], <<>>};
        {match, [Method, Path, BodyOrHeaders]} ->
            % Check if the third part is JSON (for body) or headers
            case parse_body_or_headers(BodyOrHeaders) of
                {headers, Headers} ->
                    {list_to_binary(Method), list_to_binary(Path), Headers, <<>>};
                {body, Body} ->
                    {list_to_binary(Method), list_to_binary(Path), [], Body}
            end;
        _ ->
            invalid_request
    end.

%% @doc Parse the third part of the request as either headers or body
parse_body_or_headers(String) ->
    Trimmed = string:trim(String),
    % Check if it looks like JSON
    case Trimmed of
        "{" ++ _ ->
            % It's a JSON body
            {body, list_to_binary(Trimmed)};
        _ ->
            % Assume it's headers in format "Header1: value1; Header2: value2"
            Headers = parse_headers(Trimmed),
            {headers, Headers}
    end.

%% @doc Parse header string into list of {Name, Value} tuples
parse_headers(HeaderStr) ->
    % Split by semicolons, then by colons
    HeaderParts = string:tokens(HeaderStr, ";"),
    lists:map(
        fun(Part) ->
            case string:tokens(string:trim(Part), ":") of
                [Name, Value] -> {list_to_binary(string:trim(Name)), list_to_binary(string:trim(Value))};
                _ -> {<<"invalid">>, <<"invalid">>}
            end
        end,
        HeaderParts
    ).

%% @doc Execute HTTP request using direct HTTP client (httpc)
execute_request(Method, BaseURL, Path, Headers, Body) ->
    % Build the full URL
    FullURL = <<BaseURL/binary, Path/binary>>,
    
    % Convert headers to format expected by httpc
    HttpcHeaders = [{binary_to_list(Name), binary_to_list(Value)} || {Name, Value} <- Headers],
    
    % Set up the request
    Request = case Body of
        <<>> -> {binary_to_list(FullURL), HttpcHeaders};
        _ -> {binary_to_list(FullURL), HttpcHeaders, "application/json", binary_to_list(Body)}
    end,
    
    % Execute the request directly using httpc
    try
        HttpMethod = list_to_existing_atom(string:lowercase(binary_to_list(Method))),
        case httpc:request(HttpMethod, Request, [], []) of
            {ok, {{_, StatusCode, StatusText}, ResponseHeaders, ResponseBody}} ->
                % Format response to match expected format
                {ok, #{
                    <<"status">> => StatusCode,
                    <<"status_text">> => list_to_binary(StatusText),
                    <<"headers">> => maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K, V} <- ResponseHeaders]),
                    <<"body">> => list_to_binary(ResponseBody)
                }};
            {error, Reason} ->
                {error, #{<<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}}
        end
    catch
        error:badarg ->
            % Handle the case where the atom doesn't exist
            % For HTTP methods, we should only have get, post, put, delete, head, options
            % If it's not one of these, we'll default to get
            DefaultMethod = get,
            io:format("Warning: Unsupported HTTP method '~s', using GET instead~n", [Method]),
            case httpc:request(DefaultMethod, Request, [], []) of
                {ok, {{_, SC, ST}, RH, RB}} ->
                    {ok, #{
                        <<"status">> => SC,
                        <<"status_text">> => list_to_binary(ST),
                        <<"headers">> => maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K, V} <- RH]),
                        <<"body">> => list_to_binary(RB)
                    }};
                {error, Err} ->
                    {error, #{<<"error">> => list_to_binary(io_lib:format("~p", [Err]))}}
            end
    end.

%% @doc Format HTTP response for display
format_response({Status, Response}) ->
    % Convert status to user-friendly format
    StatusStr = case Status of
        ok -> "\e[32mSuccess\e[0m";
        error -> "\e[31mError\e[0m";
        failure -> "\e[31mFailure\e[0m";
        created -> "\e[32mCreated\e[0m";
        _ -> io_lib:format("\e[33m~p\e[0m", [Status])
    end,
    
    % Format the response body
    Body = case maps:get(<<"body">>, Response, undefined) of
        undefined -> "";
        BinBody when is_binary(BinBody) ->
            % Try to pretty-print JSON if applicable
            try 
                JsonTerm = jiffy:decode(BinBody, [return_maps]),
                "\n\e[36m" ++ format_json(JsonTerm, 2) ++ "\e[0m"
            catch
                _:_ -> 
                    % Not JSON, just return as string if printable
                    case io_lib:printable_unicode_list(binary_to_list(BinBody)) of
                        true -> "\n" ++ binary_to_list(BinBody);
                        false -> io_lib:format("\n~p", [BinBody])
                    end
            end;
        Other ->
            % Try to format as term
            "\n\e[36m" ++ format_term(Other, 0) ++ "\e[0m"
    end,
    
    % Format headers if present
    HeadersStr = case maps:get(<<"headers">>, Response, undefined) of
        undefined -> "";
        Headers when is_map(Headers) ->
            "\n\e[33mHeaders:\e[0m\n" ++ 
            lists:flatten([io_lib:format("  \e[33m~s\e[0m: ~s\n", [K, V]) || {K, V} <- maps:to_list(Headers), K =/= <<"body">>]);
        _ -> ""
    end,
    
    % Combine all parts
    io_lib:format("~s ~s~s", [StatusStr, HeadersStr, Body]).

%% @doc Format JSON term with indentation
format_json(Term, Indent) when is_map(Term) ->
    case maps:size(Term) of
        0 -> "{}";
        _ ->
            Padding = lists:duplicate(Indent, " "),
            InnerPadding = lists:duplicate(Indent + 2, " "),
            KeyVals = lists:map(
                fun({K, V}) ->
                    IndentedVal = format_json(V, Indent + 2),
                    KeyStr = case is_binary(K) of
                        true -> "\"" ++ binary_to_list(K) ++ "\"";
                        false -> io_lib:format("~p", [K])
                    end,
                    InnerPadding ++ KeyStr ++ ": " ++ IndentedVal
                end,
                maps:to_list(Term)
            ),
            "{\n" ++ string:join(KeyVals, ",\n") ++ "\n" ++ Padding ++ "}"
    end;
format_json(Term, _Indent) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true -> "\"" ++ Term ++ "\"";
        false ->
            Items = lists:map(
                fun(Item) -> format_json(Item, _Indent + 2) end,
                Term
            ),
            "[" ++ string:join(Items, ", ") ++ "]"
    end;
format_json(Term, _Indent) when is_binary(Term) ->
    "\"" ++ binary_to_list(Term) ++ "\"";
format_json(Term, _Indent) ->
    io_lib:format("~p", [Term]).

%% @doc Format term for display
format_term(Term, Indent) when is_map(Term) ->
    Padding = lists:duplicate(Indent, " "),
    InnerPadding = lists:duplicate(Indent + 2, " "),
    KeyVals = lists:map(
        fun({K, V}) ->
            KeyStr = format_term(K, 0),
            ValStr = format_term(V, Indent + 2),
            InnerPadding ++ KeyStr ++ " => " ++ ValStr
        end,
        maps:to_list(Term)
    ),
    case length(KeyVals) of
        0 -> "#{} (empty map)";
        _ -> "#{\n" ++ string:join(KeyVals, ",\n") ++ "\n" ++ Padding ++ "}"
    end;
format_term(Term, _) when is_binary(Term) ->
    case io_lib:printable_unicode_list(binary_to_list(Term)) of
        true -> "<<\"" ++ binary_to_list(Term) ++ "\">>";
        false -> io_lib:format("~p", [Term])
    end;
format_term(Term, Indent) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true -> "\"" ++ Term ++ "\"";
        false ->
            Items = [format_term(I, Indent + 2) || I <- Term],
            "[" ++ string:join(Items, ", ") ++ "]"
    end;
format_term(Term, _) ->
    io_lib:format("~p", [Term]).

%% @doc Print welcome message
print_welcome(Config) ->
    Host = maps:get(host, Config),
    Port = maps:get(port, Config),
    io:format("\e[1;36m
===========================================================
==    ██████╗ ███████╗ █████╗ ███╗   ███╗               ==
==    ██╔══██╗██╔════╝██╔══██╗████╗ ████║               ==
==    ██████╔╝█████╗  ███████║██╔████╔██║               ==
==    ██╔══██╗██╔══╝  ██╔══██║██║╚██╔╝██║               ==
==    ██████╔╝███████╗██║  ██║██║ ╚═╝ ██║               ==
==    ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝               ==
==                                                       ==
==    ██╗    ██╗███████╗ █████╗ ██╗   ██╗███████╗██████╗ ==
==    ██║    ██║██╔════╝██╔══██╗██║   ██║██╔════╝██╔══██╗==
==    ██║ █╗ ██║█████╗  ███████║██║   ██║█████╗  ██████╔╝==
==    ██║███╗██║██╔══╝  ██╔══██║╚██╗ ██╔╝██╔══╝  ██╔══██╗==
==    ╚███╔███╔╝███████╗██║  ██║ ╚████╔╝ ███████╗██║  ██║==
==     ╚══╝╚══╝ ╚══════╝╚═╝  ╚═╝  ╚═══╝  ╚══════╝╚═╝  ╚═╝==
===========================================================                                  
\e[0m
\e[1;33mHyperBEAM REPL v1.0\e[0m - Connected to \e[32mhttp://~s:~p\e[0m

Type \e[36mhelp\e[0m for usage information or \e[36mexit\e[0m to quit.
Enter HTTP requests directly (e.g. \e[36mGET /~~message@1.0/hello?hello=world\e[0m)
-----------------------------------------------------------------------
", [Host, Port]).

%% @doc Print help information
print_help() ->
    HelpText = [
        "\n",
        "\e[1;33mHyperBEAM REPL Commands:\e[0m\n",
        "  \e[36mhelp\e[0m    - Show this help text\n",
        "  \e[36mclear\e[0m   - Clear the screen\n",
        "  \e[36mhistory\e[0m - Show command history\n",
        "  \e[36mexit\e[0m    - Exit the REPL\n",
        "\n",
        "\e[1;33mHTTP Request Format:\e[0m\n",
        "  \e[36mGET /path?query=value\e[0m\n",
        "  \e[36mPOST /path {\"json\":\"body\"}\e[0m\n",
        "  \e[36mPUT /path Header1: value1; Header2: value2\e[0m\n",
        "  \n",
        "\e[1;33mExamples:\e[0m\n",
        "  \e[36mGET /~meta@1.0/info/address\e[0m\n",
        "  \e[36mGET /~message@1.0/hello?hello=world\e[0m\n",
        "  \e[36mPOST /~wasm@1.0/init/compute {\"wasm-function\":\"fac\",\"wasm-params\":[5]}\e[0m\n",
        "  \n",
        "\e[1;33mUseful Paths:\e[0m\n",
        "  \e[36m/~message@1.0/keys\e[0m - List available keys in message device\n",
        "  \e[36m/~meta@1.0/info\e[0m - Get node information\n",
        "  \e[36m/~meta@1.0/devices\e[0m - List available devices\n"
    ],
    io:put_chars(HelpText),
    ok.

%% @doc Print command history
print_history(Config) ->
    History = maps:get(history, Config, []),
    case History of
        [] ->
            io:format("\e[33mNo history yet. Make some requests!\e[0m~n");
        _ ->
            io:format("\e[1;33mCommand History:\e[0m~n"),
            lists:foreach(
                fun({Idx, Cmd}) ->
                    io:format("  \e[36m~2w:\e[0m ~s~n", [Idx, Cmd])
                end,
                lists:zip(lists:seq(1, length(History)), History)
            ),
            io:format("~n"),
            ok
    end.

%% @doc Handle input for the module API
handle_input(Input) ->
    {Method, Path, Headers, Body} = parse_http_request(Input),
    Result = execute_request(Method, <<"http://localhost:10000">>, Path, Headers, Body),
    Result.