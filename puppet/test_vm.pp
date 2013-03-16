
$test_packages = [
    "htop",
    "zsh",
    "redis-server",
    "vim",
    "python-software-properties",
]

package { $test_packages:
    ensure  =>  latest,
    require =>  Exec["apt_update"]
}

exec {"/usr/bin/apt-get update":
    alias => "apt_update"
}

exec { "/usr/bin/apt-add-repository ppa:bitcoin/bitcoin || /usr/bin/apt-get update":
    alias   => "ppa_bitcoin",
    require => Package["python-software-properties"],
    creates => "/etc/apt/sources.list.d/bitcoin-bitcoin-precise.list",
}

package {"bitcoind":
    require => Exec["ppa_bitcoin"],
    ensure => latest,
}

service {"redis-server":
    require => Package["redis-server"],
    ensure => running,
    enable => true,
    hasstatus => true,
    hasrestart => true,
}

user {"bitcoind":
    ensure => present,
    home => "/home/bitcoind",
    managehome => true,
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
