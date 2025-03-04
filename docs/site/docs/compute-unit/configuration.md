# Compute Unit Configuration

The Compute Unit (CU) supports numerous environment variables for advanced configuration. This document details the available options and recommended settings.

## Configuration Methods

The Compute Unit can be configured using:

1. **Environment Variables**: Set directly in the shell or via a `.env` file
2. **Command Line Arguments**: Pass when starting the CU
3. **Configuration Files**: Use JSON configuration files

## Essential Configuration Options

### Basic Settings

| Variable | Description | Default |
|----------|-------------|---------|
| `UNIT_MODE` | Operating mode (set to "hbu" for HyperBEAM) | - |
| `HB_URL` | URL of your HyperBEAM instance | http://localhost:10000 |
| `PORT` | The port on which the CU server will listen | 6363 |
| `WALLET_FILE` | Path to your Arweave wallet JSON file | - |
| `NODE_CONFIG_ENV` | Configuration environment | "development" |

### Example .env File

A minimal configuration file looks like this:

```bash
UNIT_MODE=hbu
HB_URL=http://localhost:10000
PORT=6363
WALLET_FILE=./wallet.json
NODE_CONFIG_ENV="development"
```

## Configuration Examples

### Development Configuration

```bash
UNIT_MODE=hbu
HB_URL=http://localhost:10000
PORT=6363
WALLET_FILE=./wallet.json
NODE_CONFIG_ENV="development"
```

### Production Configuration

```bash
UNIT_MODE=hbu
HB_URL=http://localhost:10000
PORT=6363
WALLET_FILE=/secure/path/to/wallet.json
NODE_CONFIG_ENV="production"
```

## Applying Configuration Changes

For configuration changes to take effect, you need to restart the Compute Unit service. When running in development mode with hot reloading, some configuration changes may require a full restart. 