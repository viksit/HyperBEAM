# Compute Unit Overview

The ao Compute Unit (CU) is a spec-compliant implementation built with NodeJS that serves as the computational processing component in the ao ecosystem, handling WASM execution and state management.


## Architecture

The Compute Unit operates alongside HyperBEAM but runs as a separate process.

## Technical Requirements

The Compute Unit requires:

- Node.js environment
- Access to local file system for state persistence
- Network access to communicate with HyperBEAM
- An Arweave wallet for identity

## Next Steps

- [Setup](setup.md): Learn how to install and run the Compute Unit
- [Configuration](configuration.md): Understand available configuration options 