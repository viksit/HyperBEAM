# HyperBEAM Overview

HyperBEAM is a client implementation of the AO-Core protocol, written in Erlang. It serves as the 'node' software for the decentralized operating system that AO enables, abstracting hardware provisioning and details from the execution of individual programs.

## Key Features

- **Decentralized Execution**: Run AO processes in a decentralized manner
- **Message Passing**: Communicate between processes via asynchronous message passing
- **Scalable Architecture**: Built on Erlang's powerful concurrency model
- **Extensible Design**: Easy to add new devices and capabilities

## Components

HyperBEAM consists of several core components:

1. **Core Runtime**: The base system that manages process execution
2. **Device Registry**: System for registering and managing devices
3. **Message Router**: Handles message passing between processes and devices
4. **API Layer**: HTTP interfaces for interacting with the system

## System Architecture

HyperBEAM works in conjunction with the Compute Unit (CU), which handles the actual WASM execution. Together, they form a complete execution environment for AO processes.

## Next Steps

- [Setup HyperBEAM](setup.md): Instructions for installing and running HyperBEAM
- [Configuration](configuration.md): How to configure your HyperBEAM installation
- [Testing](testing.md): Run tests to verify your installation 