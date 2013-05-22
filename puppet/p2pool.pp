import "bitcoind.pp"
import "redis.pp"

class p2pool ($test = false) {

    require bitcoind
    require redis_server

    $p2pool_packages= [
        "python-zope.interface",
        "python-twisted",
        "python-twisted-web",
    ]

    package { $p2pool_packages:
        ensure  =>  latest,
    }

    user {"p2pool":
        home => "/home/p2pool",
        ensure => present,
        managehome => true,
    }

    file {"/home/p2pool/.bitcoin":
        ensure => link,
        target => "/home/bitcoind/.bitcoin",
        mode => 0644,
        owner => "p2pool",
        notify => Service["p2pool"],
    }

    vcsrepo {"/home/p2pool/p2pool":
        source   => "git://github.com/forrestv/p2pool.git",
        require  => User["p2pool"],
        user     => "p2pool",
        ensure   => "present",
        provider => git,
        alias    => "p2pool",
    }

    File {
        ensure  => present,
        mode    => 0777
    }

    file {
        "/etc/init/p2pool.conf":
            mode    => 0644,
            source  => "/configs/p2pool.conf",
            alias   => "p2pool.conf",
            notify  => Service["p2pool"];
        "/usr/bin/PoolWrapper":
            source  => "/binaries/PoolWrapper",
            alias   => "poolwrapper-binary",
            notify  => Service["p2pool"],
            owner   => "p2pool";
    }

    service {"p2pool":
        require => [
            Vcsrepo["p2pool"],
            File["p2pool.conf"],
            File["poolwrapper-binary"],
        ],
        ensure => running,
        enable => true,
    }
}

