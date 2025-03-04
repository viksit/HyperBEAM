# **HyperBEAM Repository Setup**

This guide provides step-by-step instructions for setting up and testing HyperBEAM.

## **1. Clone the HyperBEAM Repository**

First, clone the `permaweb/HyperBEAM` repository from GitHub:

```bash
git clone https://github.com/permaweb/HyperBEAM
```

Navigate to the project directory:

```bash
cd HyperBEAM
```

## **2. Compile the Code with Rebar3**

To compile the HyperBEAM code, you'll need to use Rebar3. Run the following command to compile the project:

```bash
rebar3 compile
```

This will compile the necessary code to get HyperBEAM up and running.

## **3. Run HyperBEAM with Shell**

Once the code is compiled, you can start the HyperBEAM shell with Rebar3:

```bash
rebar3 shell
```

This will start HyperBEAM using the default configuration inside the hb_opts.erl.
Which preloads all devices and sets up defaults stores. All of which can be configured using
the config.flat file with any overides you specify.

## **4. Run HyperBEAM on Mainnet**

To start HyperBEAM connected to the mainnet, you can use the `--eval` option with rebar3:

```bash
rebar3 shell --eval 'start_mainnet:start(#{}).'
```

This will start HyperBEAM with stripped opts which you can then define within the `#{}`.


