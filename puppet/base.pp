
Exec { path => [ "/bin/", "/sbin/" , "/usr/bin/", "/usr/sbin/" ] }

import "build_dep.pp"
import "redis.pp"
import "bitcoind.pp"
import "p2pool.pp"
import "bittoll.pp"

class {'ntp':}
class {'apt':}
class {'apt::unattended-upgrade::automatic':}

node "atlantis.m.bittoll.com" {
    class {'build_depends':}

    class {"redis_server":}
    class {"bitcoind":}
    class {"p2pool":}
    class {"bittoll":}
}

node "test.m.bittoll.com" {
    class {"redis_server":}
    class {"bitcoind":  test => true}
    class {"p2pool":    test => true}
    class {"bittoll":   test => true}

    import "test_vm.pp"
}

node default {
}

