
$test_packages = [
    "redis-server",
    "python-software-properties",
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


package {"bitcoind":
    require => Exec["ppa_bitcoin"],
    ensure => latest,
}

package {"libzmq1":
    require => Exec["ppa_zeromq"],
    ensure => latest,
    alias => "zeromq",
}

service {"redis-server":
    require => Package["redis-server"],
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
}

file {"/home/bitcoind/.bitcoin":
    require => User["bitcoind"],
    alias => "bitcoin_folder",
    ensure => directory,
    mode => 0644,
    owner => "bitcoind",
}

file {"/home/bitcoind/.bitcoin/bitcoin.conf":
    require => File["bitcoin_folder"],
    ensure => present,
    mode => 0644,
    source => "/configs/bitcoin.conf",
    owner => "bitcoind",
    alias => "bitcoin.conf"
}

file {"/etc/init/bitcoind.conf":
    ensure => present,
    mode => 0644,
    source => "/configs/bitcoind.conf",
    alias => "bitcoind.conf"
}

service {"bitcoind":
    require => [
        Package["bitcoind"],
        File["bitcoin.conf"],
        File["bitcoind.conf"],
    ],
    ensure => running,
    enable => true,
}

file {"/usr/bin/BCServer":
    ensure => present,
    mode => 0777,
    source => "/binaries/BCServer/BCServer",
    alias => "bc-binary",
}

file {"/usr/bin/APIServer":
    ensure => present,
    mode => 0777,
    source => "/binaries/APIServer/APIServer",
    alias => "api-binary",
}

file {"/usr/bin/PoolServer":
    ensure => present,
    mode => 0777,
    source => "/binaries/PoolServer/PoolServer",
    alias => "pool-binary",
}

file {"/etc/init/apiserver.conf":
    ensure => present,
    mode => 0644,
    source => "/configs/apiserver.conf",
    alias => "apiserver.conf"
}

file {"/etc/init/bcserver.conf":
    ensure => present,
    mode => 0644,
    source => "/configs/bcserver.conf",
    alias => "bcserver.conf"
}

file {"/etc/init/poolserver.conf":
    ensure => present,
    mode => 0644,
    source => "/configs/poolserver.conf",
    alias => "poolserver.conf"
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
    ],
}

service {"bcserver":
    require => [
        User["bcserver"],
        Package["zeromq"],
        File["bcserver.conf"],
    ],
}

service {"poolserver":
    require => [
        User["poolserver"],
        Package["zeromq"],
        File["poolserver.conf"],
    ],
}

