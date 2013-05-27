
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
import "firehol.pp"
import "config.pp"

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
    class {"firehol":       stage=>build}
    class {"config":        stage=>build}

    class {"redis_server":  stage=>install}
    class {"bitcoind":      stage=>install}
    class {"p2pool":        stage=>install}
    class {"bittoll":       stage=>install}

    Stage["build"] -> Stage["install"]
}

node "test.m.bittoll.com" {
    stage {"build":}
    stage {"install":}

    class {"config":    test => true, stage=>build}
    class {"redis_server":            stage=>install}
    class {"bitcoind":  test => true, stage=>install}
    class {"p2pool":    test => true, stage=>install}
    class {"bittoll":   test => true, stage=>install}

    class {"test_vm":                 stage=>install}

    Stage["build"] -> Stage["install"]
}

node default {
}

