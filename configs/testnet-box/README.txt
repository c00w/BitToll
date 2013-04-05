This is a private, difficulty 1 testnet in a box.
(Testnet Mark 3, as reset for the bitcoin 0.7 release)

Use it as follows:

  $ cd ~/testnet-box
  $ bitcoind -datadir=1 -daemon
  $ bitcoind -datadir=2 -daemon

This will start two nodes. You need two because otherwise the node won't
generate blocks. You now have a private testnet:

  $ bitcoind -datadir=1 getinfo
  {
    "version" : 69900,
    "protocolversion" : 60001,
    "walletversion" : 60000,
    "balance" : 2850.00000000,
    "blocks" : 732,
    "connections" : 1,
    "proxy" : "",
    "difficulty" : 1.00000000,
    "testnet" : true,
    "keypoololdest" : 1338858364,
    "keypoolsize" : 6,
    "paytxfee" : 0.00000000,
    "errors" : ""
  }

Node 1 is listening for bitcoin connections on port 19000, and for
RPC connections on port 19001.

Node 2 connects to node 1 and listens for RPC connections on
port 19011 (it does not listen for peer connections).

To start generating blocks, enable generation via the command line:

  $ bitcoind -datadir=1 help
  $ bitcoind -datadir=1 setgenerate true
or
  $ bitcoind -datadir=2 setgenerate true

To generate blocks using an external miner, point
the miner at http://{rpcuser}:{rpcpassword}@127.0.0.1:{rpcport}
For example:

  $ cd ~/DiabloMiner
  $ ./DiabloMiner-Linux.sh --url http://test:123@127.0.0.1:19001
or
  $ ./DiabloMiner-Linux.sh --url http://test:123@127.0.0.1:19011



