class bitcoind ($test = false) {

    apt::ppa {"bitcoin":
        alias   => "ppa_bitcoin",
        ensure  => present,
        key     => "8842CE5E",
        ppa     => "bitcoin";
    }

    exec {"apt-get update && touch /var/tmp/bitcoind_apt_update":
        require => [
            Apt::Ppa["ppa_bitcoin"], 
        ],
        alias   => "bitcoind_apt_update",
        creates => "/var/tmp/bitcoind_apt_update"
    }

    package {"bitcoind":
        require => [
            Apt::Ppa["ppa_bitcoin"],
            Exec["bitcoind_apt_update"],
        ],
        ensure  => latest;
    }

    user {"bitcoind":
        home    => "/home/bitcoind",
        ensure  => present,
        managehome => true,
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

    if ($test) {

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

        file {"/etc/init/bitcoind1.conf":
            ensure => present,
            mode => 0644,
            source => "/configs/bitcoind1.conf",
            alias => "bitcoind1.conf",
            notify => Service["bitcoind1"],
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
    }
    else {
        file {"/home/bitcoind/.bitcoin/bitcoin.conf":
            require => File["bitcoin_parent_folder"],
            alias   => "bitcoin_folder",
            ensure => file,
            source  => "/configs/bitcoin.conf",
            notify  => Service["bitcoind"],
            mode    => 0644,
            owner   => "bitcoind",
            group   => "bitcoind",
        }
    }

    file {"/etc/init/bitcoind.conf":
        ensure => present,
        mode => 0644,
        source => "/configs/bitcoind.conf",
        alias => "bitcoind.conf",
        notify => Service["bitcoind"],
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
}

