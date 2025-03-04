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

HyperBEAM is a distributed system that allows operators to offer computational resources to users in the AO network. It abstracts hardware provisioning details from the execution of individual programs, enabling a decentralized computing platform where:

- Programs run as independent processes
- Processes communicate via asynchronous message passing
- Computation occurs across a distributed network of nodes
- State is persisted and accessible across the network

## System Architecture

The HyperBEAM ecosystem consists of two main components:

1. **HyperBEAM**: The Erlang-based node software that handles message routing, process management, and device coordination.

2. **Compute Unit (CU)**: A NodeJS implementation that executes WebAssembly modules and handles computational tasks.

Together, these components form a complete execution environment for AO processes.

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

## Documentation Structure

This documentation is organized into the following sections:

- **[Getting Started](getting-started/index.md)**: System requirements and installation instructions
- **[HyperBEAM](hyperbeam/index.md)**: Core setup, configuration, and testing
- **[Compute Unit](compute-unit/index.md)**: Setup and configuration of the CU component
- **[Guides](guides/index.md)**: Step-by-step tutorials and walkthroughs
- **[Reference](reference/index.md)**: API documentation and troubleshooting

## Community and Support

- **GitHub**: [permaweb/HyperBEAM](https://github.com/permaweb/HyperBEAM)
- **Discord**: [Join the community](https://discord.gg/V3yjzrBxPM)
- **Issues**: [File a bug report](https://github.com/permaweb/HyperBEAM/issues)

## License

HyperBEAM is open-source software licensed under the [MIT License](https://github.com/permaweb/HyperBEAM/blob/main/LICENSE.md).
