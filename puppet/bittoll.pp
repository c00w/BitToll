import "redis.pp"
import "bitcoind.pp"

class bittoll ($test = false) {
    require redis_server
    require bitcoind

    package {
        "libzmq1":
            ensure => latest,
            alias => "zeromq";
    }

    User {
        ensure => present,
        managehome => true,
    }

    user {
        "apiserver":
            home => "/home/apiserver";
        "bcserver":
            home => "/home/bcserver";
        "poolserver":
            home => "/home/poolserver";
    }

    $mode = $test ? {
        false   => 0700,
        true    => 0777,
    }

    File {
        ensure    => present,
        mode      => $mode,
    }

    file {
        "/usr/bin/BCServer":
            source  => "/binaries/BCServer",
            alias   => "bc-binary",
            notify  => Service["bcserver"],
            owner   => "bcserver";
        "/usr/bin/APIServer":
            source  => "/binaries/APIServer",
            alias   => "api-binary",
            notify  => Service["apiserver"],
            owner   => "apiserver";
        "/usr/bin/PoolServer":
            source  => "/binaries/PoolServer",
            alias   => "pool-binary",
            notify  => Service["poolserver"],
            owner   => "poolserver";
        "/usr/bin/PoolWrapper":
            source  => "/binaries/PoolWrapper",
            alias   => "poolwrapper-binary",
            notify  => Service["p2pool"],
            owner   => "poolserver";
        "/usr/bin/NewBlock":
            source  => "/binaries/NewBlock",
            alias   => "newblock-binary",
            owner   => "bitcoind";
    }

    if ($test) {
        file {"/usr/bin/MineUserTest":
            ensure => present,
            mode => 0777,
            source => "/binaries/MineUserTest",
            alias => "mineusertest-binary",
        }
    }


    file {"/etc/init/apiserver.conf":
        ensure => present,
        mode => 0644,
        source => "/configs/apiserver.conf",
        alias => "apiserver.conf",
        notify => Service["apiserver"],
    }

    file {"/etc/init/bcserver.conf":
        ensure => present,
        mode => 0644,
        source => "/configs/bcserver.conf",
        alias => "bcserver.conf",
        notify => Service["bcserver"],
    }

    file {"/etc/init/poolserver.conf":
        ensure => present,
        mode => 0644,
        source => "/configs/poolserver.conf",
        alias => "poolserver.conf",
        notify => Service["poolserver"],
    }

    Service {
        ensure => running,
        enable => true,
    }

    service {"apiserver":
        require => [
            User["apiserver"],
            Package["zeromq"],
            File["apiserver.conf"],
            File["api-binary"],
        ],
    }

    service {"bcserver":
        require => [
            User["bcserver"],
            Package["zeromq"],
            File["bcserver.conf"],
            File["bc-binary"],
        ],
    }

    service {"poolserver":
        require => [
            User["poolserver"],
            Package["zeromq"],
            File["poolserver.conf"],
            File["pool-binary"],
        ],
    }
}
