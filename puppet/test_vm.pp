import "redis.pp"
import "bitcoind.pp"
import "bittoll.pp"

class {"redis_server":}
class {"bitcoind":
    test    => true
}

class {"bittoll":
    test    => true
}

$test_packages = [
    "python-zope.interface",
    "python-twisted",
    "python-twisted-web",
    "git",
    "vim",
    "htop",
]

package { $test_packages:
    ensure  =>  latest,
}

vcsrepo {"/home/p2pool/p2pool":
    source   => "git://github.com/forrestv/p2pool.git",
    require  => User["p2pool"],
    user     => "p2pool",
    ensure   => "present",
    provider => git,
    alias    => "p2pool",
}

User {
    ensure => present,
    managehome => true,
}

user {
    "p2pool":
        home => "/home/p2pool";
}

file {"/home/vagrant/.bitcoin":
    ensure => link,
    target => "/home/bitcoind/.bitcoin",
    mode => 0644,
    owner => "vagrant",
}

file {"/home/p2pool/.bitcoin":
    ensure => link,
    target => "/home/bitcoind/.bitcoin",
    mode => 0644,
    owner => "p2pool",
    notify => Service["p2pool"],
}

file {"/etc/init/p2pool.conf":
    ensure => present,
    mode => 0644,
    source => "/configs/p2pool.conf",
    alias => "p2pool.conf",
    notify => Service["p2pool"],
}

service {"p2pool":
    require => [
        Vcsrepo["p2pool"],
        File["p2pool.conf"],
        File["poolwrapper-binary"]
    ],
    ensure => running,
    enable => true,
}


