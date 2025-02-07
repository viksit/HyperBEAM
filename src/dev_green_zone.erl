-module(dev_green_zone).
-export([verify_and_trust/3, init/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(TEST_TRUSTED_SOFTWARE, #{
    vcpus => 1,
    vcpu_type => 5, 
    vmm_type => 1,
    guest_features => 1,
    firmware => <<"b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510">>,
    kernel => <<"69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576">>,
    initrd => <<"07ae37169b309984cb1cf8a22ac4ce36cb72eea0226ef14eb90952684734b5a3">>,
    append => <<"bc58de73c050a2cff6923e4f40311f7671398cd73159e9889398826695f84404">>
}).

%% @doc Initialize the green zone with a node keypair
init(_M1, _M2, Opts) ->
    ?event(#{
        action => init_green_zone,
        has_keypair => hb_opts:get(node_keypair, undefined, Opts) =/= undefined
    }),
    % Generate node's RSA keypair if not exists
    NodeKeyPair = case hb_opts:get(node_keypair, undefined, Opts) of
        undefined -> 
            ?event(#{action => generating_new_keypair}),
            crypto:generate_key(rsa, {4096, 65537});
        ExistingKey -> ExistingKey
    end,

    ok = hb_http_server:set_opts(Opts#{
        % Store the node's keypair
        node_keypair => NodeKeyPair
    }),
    {ok, <<"Green zone initialized successfully.">>}.

%% @doc Verify a node's TEE attestation report and establish secure channel
verify_and_trust(M1, _M2, Opts) ->
    ?event(#{
        action => verify_and_trust_start,
        node_address => hb_converge:get(<<"node-address">>, M1, Opts),
        message => M1,
        opts => Opts
    }),
    % Get the attestation report and requester's public key from the request
    Report = hb_converge:get(<<"report">>, M1, Opts),
    NodeAddr = hb_converge:get(<<"address">>, M1, Opts),
    NodeMsg = hb_converge:get(<<"node-message">>, M1, Opts),
    EncodedPubKey = hb_converge:get(<<"public-key">>, NodeMsg, Opts),
    RequesterPubKey = case EncodedPubKey of
        not_found -> not_found;
        Encoded -> binary_to_term(base64:decode(Encoded))
    end,
    
    ?event(#{
        action => got_public_key,
        encoded_key => EncodedPubKey,
        decoded_key => RequesterPubKey,
        message_keys => maps:keys(M1)
    }),

    ?event(#{
        action => extracted_values,
        report_size => byte_size(Report),
        node_addr => NodeAddr,
        requester_pub_key => RequesterPubKey
    }),

    % Verify the attestation report using dev_snp
    case dev_snp:verify(M1, #{<<"target">> => <<"self">>}, Opts#{trusted => ?TEST_TRUSTED_SOFTWARE}) of
        {ok, true} ->
            ?event(#{
                action => attestation_verified,
                node_address => NodeAddr
            }),
            % Generate AES key for secure communication
            AESKey = crypto:strong_rand_bytes(32),
            
            % Get our node's keypair
            {PubKey, _PrivKey} = hb_opts:get(node_keypair, undefined, Opts),
            
            % Get current trusted nodes
            TrustedNodes = hb_opts:get(trusted_nodes, #{}, Opts),
            
            % Add the verified node to trusted nodes with its AES key
            UpdatedTrustedNodes = maps:put(NodeAddr, #{
                report => Report,
                aes_key => AESKey,
                public_key => RequesterPubKey
            }, TrustedNodes),
            
            ok = hb_http_server:set_opts(Opts#{
                trusted_nodes => UpdatedTrustedNodes
            }),
            
            % Encrypt AES key with requester's public key
            EncryptedPayload = encrypt_payload(AESKey, RequesterPubKey),
            
            ?event(#{
                action => node_trusted,
                node_address => NodeAddr,
                aes_key_size => byte_size(AESKey)
            }),
            {ok, #{
                <<"status">> => <<"success">>,
                <<"message">> => <<"Node verified and added to trusted nodes">>,
                <<"node_address">> => NodeAddr,
                <<"encrypted_payload">> => EncryptedPayload,
                <<"public_key">> => PubKey
            }};
        {ok, false} ->
            ?event(#{
                action => attestation_failed,
                node_address => NodeAddr,
                reason => invalid_report
            }),
            {error, <<"Invalid attestation report">>};
        Error ->
            ?event(#{
                action => attestation_error,
                node_address => NodeAddr,
                error => Error
            }),
            Error
    end.

%% @private Encrypt the AES key using the requester's public key
encrypt_payload(AESKey, RequesterPubKey) ->
    ?event(#{
        action => encrypting_payload_start,
        aes_key_size => byte_size(AESKey),
        requester_pub_key => RequesterPubKey
    }),

    case RequesterPubKey of
        not_found ->
            ?event(#{
                action => public_key_missing,
                error => public_key_not_found
            }),
            error(public_key_not_found);
            
        {{rsa, E}, N} ->
            ?event(#{
                action => rsa_key_components,
                exponent => E,
                modulus_size => byte_size(N)
            }),
            % Create the payload containing AES key
            Payload = AESKey,
            
            % Convert to crypto RSA format
            Key = [E, crypto:bytes_to_integer(N)],
            
            % Use RSA encryption with no padding
            EncryptedResult = crypto:public_encrypt(rsa, Payload, Key, []),
            
            ?event(#{
                action => encryption_complete,
                encrypted_size => byte_size(EncryptedResult)
            }),
            EncryptedResult;
        Other ->
            ?event(#{
                action => invalid_key_format,
                got => Other
            }),
            error({invalid_key_format, Other})
    end.

%% Tests

verify_and_trust_test() ->
    ?event(#{action => starting_verify_and_trust_test}),
    
    % Create wallet and get address like in dev_snp:generate
    Wallet = ar_wallet:new(),
    NodeAddr = hb_util:human_id(ar_wallet:to_address(Wallet)),
    {PrivKey, PubKey} = Wallet,
    {{rsa, E}, N, D} = PrivKey,  % Extract RSA components
    
    ?event(#{
        action => wallet_created,
        node_addr => NodeAddr,
        key_info => #{
            type => rsa,
            exponent => E,
            n_size => byte_size(N),
            d_size => byte_size(D)
        },
        priv_key => #{
            format => {{rsa, E}, N, D}
        },
        pub_key => PubKey
    }),

    % Start the HTTP server with our test configuration
    ServerOpts = #{
        force_signed => true,
        trusted => ?TEST_TRUSTED_SOFTWARE,
        priv_wallet => Wallet
    },
    _ServerAddr = hb_http_server:start_node(ServerOpts),

    % Get mock attestation report from test file
    {ok, Report} = file:read_file("test/green_zone_report.json"),

    % Create test messages with properly formatted IDs
    NodeMsg = #{
        <<"attestors">> => <<"none">>,
        <<"public-key">> => base64:encode(term_to_binary(PubKey))  % Encode the public key
    },
    ?event(#{
        action => node_msg_created,
        node_msg => NodeMsg
    }),

    {ok, NodeMsgId} = dev_message:id(NodeMsg, #{}, #{}),

    % Generate nonce from address and message ID
    RawAddr = hb_util:native_id(NodeAddr),
    RawMsgId = hb_util:native_id(NodeMsgId),
    Nonce = <<RawAddr/binary, RawMsgId/binary>>,

    M1 = hb_message:attest(#{
        <<"report">> => Report,
        <<"address">> => NodeAddr,
        <<"node-message">> => NodeMsg,  % NodeMsg now contains the public key
        <<"nonce">> => hb_util:encode(Nonce),
        % Add trusted software parameters
        <<"vcpus">> => 1,
        <<"vcpu_type">> => 5,
        <<"vmm_type">> => 1,
        <<"guest_features">> => 1,
        <<"firmware">> => <<"b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510">>,
        <<"kernel">> => <<"69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576">>,
        <<"initrd">> => <<"07ae37169b309984cb1cf8a22ac4ce36cb72eea0226ef14eb90952684734b5a3">>,
        <<"append">> => <<"bc58de73c050a2cff6923e4f40311f7671398cd73159e9889398826695f84404">>
    }, Wallet),

    ?event(#{
        action => message_created,
        m1_raw => M1,
        m1_keys => maps:keys(M1),
        node_msg_in_m1 => hb_converge:get(<<"node-message">>, M1, #{}),
        pub_key_sent => PubKey
    }),

    M2 = #{
        <<"target">> => <<"self">>
    },

    % Create test options with trusted software configuration
    Opts = #{
        node_keypair => Wallet,  % Use the wallet as the keypair
        trusted => ?TEST_TRUSTED_SOFTWARE,
        http_server => NodeAddr
    },

    % Call verify_and_trust
    Result = verify_and_trust(M1, M2, Opts),

    % Verify the result
    ?assertMatch({ok, #{
        <<"status">> := <<"success">>,
        <<"message">> := <<"Node verified and added to trusted nodes">>,
        <<"node_address">> := NodeAddr,
        <<"encrypted_payload">> := _,
        <<"public_key">> := _
    }}, Result),

    ?event(#{
        action => test_completed,
        result => Result
    }).

