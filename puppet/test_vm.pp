$test_packages = [
    "python-software-properties",
    "python-zope.interface",
    "python-twisted",
    "python-twisted-web",
    "git",
    "vim",
    "htop",
]

package { $test_packages:
    ensure  =>  latest,
    require =>  Exec["apt_update"]
}

exec {"/usr/bin/apt-get update":
    alias => "apt_update"
}

exec { "/usr/bin/apt-add-repository ppa:bitcoin/bitcoin && /usr/bin/apt-get update":
    alias   => "ppa_bitcoin",
    require => Package["python-software-properties"],
    creates => "/etc/apt/sources.list.d/bitcoin-bitcoin-precise.list",
}

exec { "/usr/bin/apt-add-repository ppa:chris-lea/zeromq && /usr/bin/apt-get update":
    alias   => "ppa_zeromq",
    require => Package["python-software-properties"],
    creates => "/etc/apt/sources.list.d/chris-lea-zeromq-precise.list",
}

exec { "/usr/bin/apt-add-repository ppa:chris-lea/redis-server && /usr/bin/apt-get update":
    alias   => "ppa_redis",
    require => Package["python-software-properties"],
    creates => "/etc/apt/sources.list.d/chris-lea-redis-server-precise.list",
}

package {"redis-server":
    require => [
        Exec["ppa_redis"],
    ],
    ensure => latest,
}


package {"bitcoind":
    require => Exec["ppa_bitcoin"],
    ensure => latest,
}

package {"libzmq1":
    require => Exec["ppa_zeromq"],
    ensure => latest,
    alias => "zeromq",
}

vcsrepo {"/home/p2pool/p2pool":
    source   => "git://github.com/forrestv/p2pool.git",
    require  => User["p2pool"],
    user     => "p2pool",
    ensure   => latest,
    provider => git,
    alias    => "p2pool",
}

service {"redis-server":
    require => [
        Package["redis-server"],
        File["/etc/redis/redis.conf"],
    ],
    ensure => running,
    enable => true,
    hasstatus => true,
    hasrestart => true,
}

User {
    ensure => present,
    managehome => true,
}

user {
    "bitcoind":
        home => "/home/bitcoind";
    "apiserver":
        home => "/home/apiserver";
    "bcserver":
        home => "/home/bcserver";
    "poolserver":
        home => "/home/poolserver";
    "p2pool":
        home => "/home/p2pool";
}

file {"/etc/redis/redis.conf":
    alias => "redis.conf",
    ensure => present,
    mode => 0644,
    owner => root,
    source => "/configs/redis.conf",
    notify => Service["redis-server"],
}

file {"/home/bitcoind/.bitcoin":
    require => User["bitcoind"],
    alias   => "bitcoin_folder",
    ensure  => directory,
    mode    => 0600,
    owner   => "bitcoind",
    notify  => Service["bitcoind"],
    recurse => true,
    source  => "/configs/testnet-box/1/",
}

file {"/home/vagrant/.bitcoin":
    require => File["bitcoin_folder"],
    ensure => link,
    target => "/home/bitcoind/.bitcoin",
    mode => 0644,
    owner => "vagrant",
}

file {"/home/p2pool/.bitcoin":
    require => [
        File["bitcoin_folder"],
        User["p2pool"],
    ],
    ensure => link,
    target => "/home/bitcoind/.bitcoin",
    mode => 0644,
    owner => "p2pool",
    notify => Service["p2pool"],
}

file {"/etc/init/bitcoind.conf":
    ensure => present,
    mode => 0644,
    source => "/configs/bitcoind.conf",
    alias => "bitcoind.conf",
    notify => Service["bitcoind"],
}

file {"/etc/init/p2pool.conf":
    ensure => present,
    mode => 0644,
    source => "/configs/p2pool.conf",
    alias => "p2pool.conf",
    notify => Service["p2pool"],
}

service {"bitcoind":
    require => [
        Package["bitcoind"],
        File["bitcoin_folder"],
        File["bitcoind.conf"],
    ],
    ensure => running,
    enable => true,
}

service {"p2pool":
    require => [
        Vcsrepo["p2pool"],
        File["p2pool.conf"],
    ],
    ensure => running,
    enable => true,
}



file {"/usr/bin/BCServer":
    ensure => present,
    mode => 0777,
    source => "/binaries/BCServer/BCServer",
    alias => "bc-binary",
    notify => Service["bcserver"],
}

file {"/usr/bin/APIServer":
    ensure => present,
    mode => 0777,
    source => "/binaries/APIServer/APIServer",
    alias => "api-binary",
    notify => Service["apiserver"],
}

file {"/usr/bin/PoolServer":
    ensure => present,
    mode => 0777,
    source => "/binaries/PoolServer/PoolServer",
    alias => "pool-binary",
    notify => Service["poolserver"],
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

