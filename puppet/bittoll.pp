import "chris_ppa.pp"
import "config.pp"
import "p2pool.pp"
import "bitcoind.pp"
import "redis.pp"

class base {
    require chris_ppa
    require config

    package {"libzmq1":
        ensure  => "3.2.2-1chl1~precise1",
        alias   => "zeromq";
    }

}

class apiserver {
    require base

    user {"apiserver":
        ensure      => present,
        home        => "/home/apiserver",
        managehome  => true,
        password    => '*',
    }

    file {"/usr/bin/APIServer":
        ensure  => present,
        mode    => 0500,
        source  => "/binaries/APIServer",
        alias   => "api-binary",
        notify  => Service["apiserver"],
        owner   => "apiserver",
    }

    file {"/etc/init/apiserver.conf":
        ensure => present,
        mode => 0644,
        source => "/configs/apiserver.conf",
        alias => "apiserver.conf",
        notify => Service["apiserver"],
    }

    service {"apiserver":
        require => [
            User["apiserver"],
            File["apiserver.conf"],
            File["api-binary"],
        ],
        ensure  => running,
        enable  => true,
    }
}

class poolwrapper {
    require base
    require bcserver
    require redis_server

    file {"/usr/bin/PoolWrapper":
        require => Package["libzmq1"],
        source  => "/binaries/PoolWrapper",
        alias   => "poolwrapper-binary",
        notify  => Service["p2pool"],
        owner   => "root",
        ensure  => present,
        mode    => 0555,
    }
}

class bcserver {
    require base
    require bitcoind

    user {"bcserver":
        ensure      => present,
        home        => "/home/bcserver",
        managehome  => true,
        password    => '*',
    }

    file {"/usr/bin/BCServer":
        ensure  => present,
        mode    => 0500,
        source  => "/binaries/BCServer",
        alias   => "bc-binary",
        notify  => Service["bcserver"],
        owner   => "bcserver",
    }

    file {"/etc/init/bcserver.conf":
        ensure  => present,
        mode    => 0644,
        source  => "/configs/bcserver.conf",
        alias   => "bcserver.conf",
        notify  => Service["bcserver"],
    }

    service {"bcserver":
        require     => [
            User["bcserver"],
            File["bcserver.conf"],
            File["bc-binary"],
        ],
        subscribe   => Service["bitcoind"],
        ensure      => running,
        enable      => true,
    }
}

class poolserver () {

    require base
    require p2pool

    user {"poolserver":
        ensure      => present,
        home        => "/home/poolserver",
        managehome  => true,
        password    => '*',
    }

    file {"/usr/bin/PoolServer":
        ensure  => present,
        source  => "/binaries/PoolServer",
        alias   => "pool-binary",
        notify  => Service["poolserver"],
        owner   => "poolserver",
        mode    => 0500,
    }

    file {"/etc/init/poolserver.conf":
        ensure => present,
        mode => 0644,
        source => "/configs/poolserver.conf",
        alias => "poolserver.conf",
        notify => Service["poolserver"],
    }

    service {"poolserver":
        require => [
            User["poolserver"],
            Package["zeromq"],
            File["poolserver.conf"],
            File["pool-binary"],
        ],
        ensure  => running,
        enable  => true,
        subscribe => Service["p2pool"],
    }
}

class bittoll ($test = false) {

    require apiserver
    require bcserver
    require poolserver

    if ($test) {
        file {"/usr/bin/MineUserTest":
            ensure => present,
            mode => 0777,
            source => "/binaries/MineUserTest",
            alias => "mineusertest-binary",
        }
    }
}
