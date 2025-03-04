# **Local CU Setup**

This guide explains how to set up the local Compute Unit (CU) for HyperBEAM.

## What is Local CU?

The ao Compute Unit (CU) is a spec-compliant implementation built with NodeJS. It serves as the computational processing component in the ao ecosystem, handling WASM execution and state management.

## Prerequisites

* Node.js
* Git

## Installation Steps

### 1. Clone the Repository

```bash
git clone https://github.com/permaweb/ao.git
cd ao/servers/cu
```

### 2. Install Dependencies

```bash
npm i
```

### 3. Generate a Wallet (if needed)

If you don't already have an Arweave wallet, you can generate one:

```bash
npx --yes @permaweb/wallet > wallet.json
```

### 4. Configure Environment

Create a .env file with the following minimal configuration:

```bash
UNIT_MODE=hbu
HB_URL=http://localhost:10000
PORT=6363
WALLET_FILE=./wallet.json
NODE_CONFIG_ENV="development"
```

### 5. Start the Compute Unit

```bash
npm start
```

For development with hot-reloading, you can use:

```bash
npm run dev
```

## Key Configuration Options

The CU supports numerous environment variables for advanced configuration. Here are some important ones:

* **WALLET_FILE**: Path to your Arweave wallet JSON file
* **PORT**: The port on which the CU server will listen (default: 6363)
* **UNIT_MODE**: Set to "hbu" for hyperBEAM mode
* **HB_URL**: URL of your hyperBEAM instance
* **PROCESS_WASM_MEMORY_MAX_LIMIT**: Maximum memory limit for processes (default: 1GB)
* **DEFAULT_LOG_LEVEL**: Logging level (error, warn, info, http, verbose, debug, silly)

## Verification

To verify that your CU is running correctly, you can check:

```bash
curl http://localhost:6363
```
