
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
import "nginx.pp"
import "statsd.pp"
import "graphite.pp"
import "uwsgi.pp"

class {"ntp":}
class {"apt":}
class {"apt::unattended-upgrade::automatic":}

stage {"build":}
stage {"install":}
Stage["build"] -> Stage["install"]

node "atlantis.m.bittoll.com" {

    class {"build_depends": stage=>build}
    class {"build":         stage=>build}
    class {"links":         stage=>build}
    class {"production":    stage=>build}
    class {"firehol":       stage=>build}

    class {"statsd":        stage=>build}
    class {"graphite":      stage=>build}
    class {"uwsgi":         stage=>build}


    class {"config":}

    class {"redis_server":  stage=>install}
    class {"bitcoind":      stage=>install}
    class {"p2pool":        stage=>install}
    class {"bittoll":       stage=>install}
    class {"nginx":         stage=>install}

    Class["bittoll"] -> Class["nginx"]
    Stage["build"] -> Class["config"]
    Class["config"] -> Stage["install"]
}

node "testlive.m.bittoll.com" {

    class {"config":}

    class {"redis_server":  stage=>install}
    class {"bitcoind":      stage=>install}
    class {"p2pool":        stage=>install}
    class {"bittoll":       stage=>install}
    class {"nginx":         stage=>install}

    Class["bittoll"] -> Class["nginx"]
    Stage["build"] -> Class["config"]
    Class["config"] -> Stage["install"]
}

node "test.m.bittoll.com" {

    #class {"statsd":        stage=>build}
    #class {"graphite":      test=>true, stage=>build}
    #class {"uwsgi":         test=>true, stage=>build}

    class {"config":        test=>true}
    class {"redis_server":              stage=>install}
    class {"bitcoind":      test=>true, stage=>install}
    class {"p2pool":        test=>true, stage=>install}
    class {"bittoll":       test=>true, stage=>install}
    class {"nginx":         test=>true, stage=>install}
    class {"test_vm":       stage=>install}

    Stage["build"] -> Class["config"]
    Class["config"] -> Stage["install"]
}

node default {
}

