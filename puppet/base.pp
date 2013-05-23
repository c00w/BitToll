
Exec { path => [ "/bin/", "/sbin/" , "/usr/bin/", "/usr/sbin/" ] }

import "build_dep.pp"
import "redis.pp"
import "bitcoind.pp"
import "p2pool.pp"
import "bittoll.pp"
import "build.pp"
import "links.pp"
import "test_vm.pp"
import "production.pp"

class {"ntp":}
class {"apt":}
class {"apt::unattended-upgrade::automatic":}

node "atlantis.m.bittoll.com" {
    stage {"build":}
    stage {"install":}

    class {"build_depends": stage=>build}
    class {"build":         stage=>build}
    class {"links":         stage=>build}
    class {"production":    stage=>build}

    class {"redis_server":  stage=>install}
    class {"bitcoind":      stage=>install}
    class {"p2pool":        stage=>install}
    class {"bittoll":       stage=>install}

    Stage["build"] -> Stage["install"]
}

node "test.m.bittoll.com" {
    class {"redis_server":}
    class {"bitcoind":  test => true}
    class {"p2pool":    test => true}
    class {"bittoll":   test => true}

    class {"test_vm":}
}

node default {
}

