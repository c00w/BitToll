import "bitcoind.pp"
import "bittoll.pp"
import "git.pp"

class p2pool ($test = false) {

    require git
    require bitcoind
    require poolwrapper

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

    $source = $test ? {
        true    => "/configs/test/p2pool.conf",
        false   => "/configs/p2pool.conf",
    }

    file {
        "/etc/init/p2pool.conf":
            mode    => 0644,
            source  => $source,
            alias   => "p2pool.conf",
            notify  => Service["p2pool"];
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

