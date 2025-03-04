<div class="header-main">
	<div class="header-logo">
		<div class="logo-container">
		<div class="logo-stripes">
			<div class="stripe green"></div>
			<div class="stripe yellow"></div>
			<div class="stripe blue"></div>
			<div class="stripe purple"></div>
			<div class="stripe red"></div>
		</div>
		<div class="logo-text">
			<h1>hyperBEAM.</h1>
			<p class="tagline">DOCUMENTATION</p>
		</div>
		</div>
	</div>
</div>

!!! warning "Platform Support"
    This documentation is currently written specifically for **Ubuntu 22.04**. Support for macOS and other platforms will be added in future updates.

Welcome to the official documentation for HyperBEAM, a client implementation of the AO-Core protocol written in Erlang. HyperBEAM serves as the node software for the decentralized operating system that AO enables.

## What is HyperBEAM?

HyperBEAM is a client implementation of the AO-Core protocol, written in Erlang. It serves as the 'node' software for the decentralized operating system that AO enables, abstracting hardware provisioning details from the execution of individual programs.

HyperBEAM node operators can offer computational resources to users in the AO network by electing to execute any number of different devices, potentially charging users for their computation as necessary. It enables a decentralized computing platform where:

- Programs run as independent processes
- Processes communicate via asynchronous message passing
- Computation occurs across a distributed network of nodes
- State is persisted and accessible across the network

### What is AO-Core?

AO-Core is a protocol built to enable decentralized computations, offering a series of universal primitives. Instead of enforcing a single, monolithic architecture, AO-Core provides a framework into which any number of different computational models, encapsulated as primitive devices, can be attached.

AO-Core's protocol is built upon these fundamental primitives:

- **Hashpaths**: A mechanism for referencing locations in a program's state-space prior to execution
- **Unified data structure**: For representing program states as HTTP documents
- **Attestation protocol**: For expressing attestations of states found at particular hashpaths
- **Meta-VM**: Allowing various virtual machines and computational models (devices) to be executed inside the AO-Core protocol

## System Architecture

The HyperBEAM ecosystem consists of two main components:

1. **HyperBEAM**: The Erlang-based node software that handles message routing, process management, and device coordination.

2. **Compute Unit (CU)**: A NodeJS implementation that executes WebAssembly modules and handles computational tasks.

Together, these components form a complete execution environment for AO processes.

### Messages

HyperBEAM describes every piece of data as a message, which can be interpreted as a binary term or as a collection of named functions (a Map of functions). Every message may specify a device which is interpreted by the AO-Core compatible system to operate upon the message's contents.

Executing a named function within a message, providing a map of arguments, results in another message. In this way, messages in AO-Core always beget further messages, giving rise to a vast computational space leveraging function application and composition at its core.

### Devices

HyperBEAM supports numerous devices, each enabling different services to be offered by the node. Each HyperBEAM node comes with preloaded devices, including:

- **~meta@1.0**: Used to configure the node's hardware, supported devices, and other settings
- **~relay@1.0**: Used to relay messages between nodes and the wider HTTP network
- **~wasm64@1.0**: Used to execute WebAssembly code
- **~process@1.0**: Enables creation of persistent, shared executions accessible by multiple users
- **~snp@1.0**: Used for trusted execution environment (TEE) operations

Node operators can add or remove devices as necessary based on their requirements.

## Key Features

- **Decentralized Execution**: Run processes in a distributed environment
- **Device System**: Register and use a variety of devices for extended functionality
- **Message Passing**: Built on an asynchronous message passing architecture
- **Scalable Architecture**: Built on Erlang's powerful concurrency model
- **Extensible Design**: Easily add new devices and capabilities

## Quick Start

To get started with HyperBEAM:

1. [Check system requirements](getting-started/requirements.md)
2. [Install dependencies](getting-started/installation/index.md)
3. [Set up HyperBEAM](hyperbeam/setup.md)
4. [Configure the Compute Unit](compute-unit/setup.md)
5. [Verify your installation](guides/integration.md)

### Basic Installation Requirements

You will need:

- The Erlang runtime (OTP 27)
- Rebar3
- Rust / Cargo
- Git

## Documentation Structure

This documentation is organized into the following sections:

- **[Getting Started](getting-started/index.md)**: System requirements and installation instructions
- **[HyperBEAM](hyperbeam/index.md)**: Core setup, configuration, and testing
- **[Compute Unit](compute-unit/index.md)**: Setup and configuration of the CU component
- **[Guides](guides/index.md)**: Step-by-step tutorials and walkthroughs
- **[Reference](reference/index.md)**: API documentation and troubleshooting

## Community and Support

- **GitHub HyperBEAM**: [permaweb/HyperBEAM](https://github.com/permaweb/HyperBEAM)
- **Github Local CU**: [permaweb/local-cu](https://github.com/permaweb/local-cu)
- **Discord**: [Join the community](https://discord.gg/V3yjzrBxPM)
- **Issues**: [File a bug report](https://github.com/permaweb/HyperBEAM/issues)

## License

HyperBEAM is open-source software licensed under the [MIT License](https://github.com/permaweb/HyperBEAM/blob/main/LICENSE.md).
