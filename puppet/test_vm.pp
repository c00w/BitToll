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

apt::ppa {
    'bitcoin':
        alias   => "ppa_bitcoin",
        ensure  => present,
        key     => "8842CE5E",
        ppa     => "bitcoin";
    'chris-lea':
        alias   => "ppa_zeromq_redis",
        ensure  => present,
        key     => "C7917B12",
        ppa     => ["zeromq", "redis-server"]
}

exec {"apt-get update && touch /var/tmp/apt_update":
    require => [
        Apt::Ppa["ppa_zeromq_redis"],
        Apt::Ppa["ppa_bitcoin"],
    ],
    path    => "/usr/bin",
    alias   => "apt_update",
    creates => "/var/tmp/apt_update",
}

package {
    "redis-server":
        require => [
            Apt::Ppa["ppa_zeromq_redis"],
            Exec["apt_update"],
        ],
        ensure  => latest;
    "bitcoind":
        require => [
            Apt::Ppa["ppa_bitcoin"],
            Exec["apt_update"],
        ],
        ensure => latest;
    "libzmq1":
        require => [
            Apt::Ppa["ppa_zeromq_redis"],
            Exec["apt_update"],
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
    alias   => "redis.conf",
    ensure  => present,
    mode    => 0644,
    owner   => root,
    source  => "/configs/redis.conf",
    require => Apt::Ppa["ppa_zeromq_redis"],
    notify  => Service["redis-server"],
}

file {"/home/bitcoind/.bitcoin":
    require => [
        User["bitcoind"],
        ],
    alias   => "bitcoin_parent_folder",
    ensure  => directory,
    mode    => 0644,
    owner   => "bitcoind",
    group   => "bitcoind",
}

file {"/home/bitcoind/.bitcoin/bitcoin.conf":
    require => File["bitcoin_parent_folder"],
    alias   => "bitcoin_folder_conf",
    ensure => file,
    source  => "/configs/testnet-box/1/bitcoin.conf",
    notify  => Service["bitcoind"],
    mode    => 0644,
    owner   => "bitcoind",
    group   => "bitcoind",
}

file {"/home/bitcoind/.bitcoin/testnet3/":
    require => File["bitcoin_folder_conf"],
    alias   => "bitcoin_folder",
    ensure  => directory,
    mode    => 0600,
    owner   => "bitcoind",
    group   => "bitcoind",
    recurse => true,
    #purge   => true,
    #force   => true,
    replace => false,
    source  => "/configs/testnet-box/1/testnet3",
}

file {"/home/bitcoind/.bitcoin1":
    require => [
        User["bitcoind"],
        ],
    alias   => "bitcoin_folder1",
    ensure  => directory,
    mode    => 0600,
    owner   => "bitcoind",
    group   => "bitcoind",
    notify  => Service["bitcoind1"],
    recurse => true,
    #purge   => true,
    #force   => true,
    replace => false,
    source  => "/configs/testnet-box/2/",
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

file {"/etc/init/bitcoind1.conf":
    ensure => present,
    mode => 0644,
    source => "/configs/bitcoind1.conf",
    alias => "bitcoind1.conf",
    notify => Service["bitcoind1"],
}

file {"/etc/init/p2pool.conf":
    ensure => present,
    mode => 0644,
    source => "/configs/p2pool.conf",
    alias => "p2pool.conf",
    notify => Service["p2pool"],
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

service {"bitcoind":
    require => [
        Package["bitcoind"],
        File["bitcoin_folder"],
        File["bitcoind.conf"],
    ],
    ensure => running,
    enable => true,
}

service {"bitcoind1":
    require => [
        Package["bitcoind"],
        File["bitcoin_folder1"],
        File["bitcoind1.conf"],
    ],
    ensure => running,
    enable => true,
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

