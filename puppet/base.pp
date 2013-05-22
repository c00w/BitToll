
Exec { path => [ "/bin/", "/sbin/" , "/usr/bin/", "/usr/sbin/" ] }

import "build.pp"

class {'ntp':}
class {'apt':}
class {'apt::unattended-upgrade::automatic':}

node "atlantis.m.bittoll.com" {
    class {'build_depends':}
    import "test_vm.pp"
}

node "test.m.bittoll.net" {
    import "test_vm.pp"
}

node default {

}

