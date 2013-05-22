
import "build.pp"

class {'ntp':}
class {'apt':}
class {'apt::unattended-upgrade::automatic':}

node atlantis.m.bittoll.com {
    class {'build_depends':}
    import "test_vm.pp"
}

node default {
    import "test_vm.pp"
}

