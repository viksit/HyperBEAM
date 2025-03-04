# HyperBEAM Configuration

HyperBEAM can be configured using a variety of methods and options. This document covers the configuration options available and how to use them.

## Configuration Methods

HyperBEAM can be configured in several ways:

1. **Command Line Arguments**: Pass configuration when starting HyperBEAM
2. config file todo
3. **Environment Variables**: Set options via environment variables

## Basic Configuration

The most common way to configure HyperBEAM is via command line arguments when starting the node:

```bash
rebar3 shell --eval "hb:start_mainnet(#{ port => 9001, key_location => 'path/to/my/wallet.key' })."
```

## Configuration Options

Here are the main configuration options available:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `port` | Integer | 10000 | HTTP API port |
| `key_location` | String | - | Path to operator wallet key file |
| `devices` | List | [] | Additional devices to load |

## Using a Configuration File

Node operators can also configure using a flat@1.0 encoded settings file. Example format:

```yaml
port: 10000
```

Save this to a file (e.g., `config.flat`) and load it when starting HyperBEAM:

```bash
rebar3 shell --eval "hb:start_mainnet(#{hb_config_location => \"config.flat\"})."
```

## Advanced Configuration

For advanced use cases, HyperBEAM supports additional configuration options:
Todo

## Environment Variables

HyperBEAM also respects certain environment variables:

```bash
export HB_PORT=9001
export HB_KEY=/path/to/wallet.key
export HB_CONFIG=config.flat
export HB_STORE=/path/to/store
export HB_MODE=debug 					#Options debug | prod
export HB_PRINT=dev_meta 				#Comma seperatted values
```

## Recommended Configurations

For most users, the following configuration is recommended:

```yaml
port: 10000
```
