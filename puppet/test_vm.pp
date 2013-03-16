
$test_packages = [
    "htop",
    "zsh",
    "redis-server",
    "vim",
    "python-software-properties",
]

package { $test_packages:
    ensure  =>  latest,
}

exec { "apt-add-repository ppa:bitcoin/bitcoin":
    alias   =>  "ppa_bitcoin",
    require => Package["python-software-properties"],
    creates => "/etc/apt/sources.list.d/bitcoin-bitcoin-precise.list"
}

package {"bitcoind":
    require => Exec["ppa_bitcoin"],
    ensure => latest,
}

service {"redis-server":
    requires => Package["redis-server"],
    ensure => running,
    enable => true,
}

user {"bitcoind":
    ensure => present,
    managehome => present,
}


