import "redis.pp"
import "bitcoind.pp"
class {"redis_server":}
class {"bitcoind":
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

package {
    "libzmq1":
        require => [
            Class["redis_server"]
        ],
        ensure => latest,
        alias => "zeromq";
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
    "apiserver":
        home => "/home/apiserver";
    "bcserver":
        home => "/home/bcserver";
    "poolserver":
        home => "/home/poolserver";
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

file {"/usr/bin/BCServer":
    ensure => present,
    mode => 0777,
    source => "/binaries/BCServer",
    alias => "bc-binary",
    notify => Service["bcserver"],
}

file {"/usr/bin/APIServer":
    ensure => present,
    mode => 0777,
    source => "/binaries/APIServer",
    alias => "api-binary",
    notify => Service["apiserver"],
}

file {"/usr/bin/PoolServer":
    ensure => present,
    mode => 0777,
    source => "/binaries/PoolServer",
    alias => "pool-binary",
    notify => Service["poolserver"],
}

file {"/usr/bin/PoolWrapper":
    ensure => present,
    mode => 0777,
    source => "/binaries/PoolWrapper",
    alias => "poolwrapper-binary",
    notify => Service["p2pool"],
}

file {"/usr/bin/NewBlock":
    ensure => present,
    mode => 0777,
    source => "/binaries/NewBlock",
    alias => "newblock-binary",
}

file {"/usr/bin/MineUserTest":
    ensure => present,
    mode => 0777,
    source => "/binaries/MineUserTest",
    alias => "mineusertest-binary",
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

