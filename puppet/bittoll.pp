import "chris_ppa.pp"
import "config.pp"

class bittoll ($test = false) {

    require chris_ppa
    require config

    package {
        "libzmq1":
            ensure  => "3.2.2-1chl1~precise1",
            alias   => "zeromq";
        "libcap2-bin":
            ensure  => "present"
    }

    User {
        ensure      => present,
        managehome  => true,
        password    => '*',
    }

    user {
        "apiserver":
            home => "/home/apiserver";
        "bcserver":
            home => "/home/bcserver";
        "poolserver":
            home => "/home/poolserver";
    }

    $mode = $test ? {
        false   => 0700,
        true    => 0777,
    }

    File {
        ensure    => present,
        mode      => $mode,
    }

    file {
        "/usr/bin/BCServer":
            source  => "/binaries/BCServer",
            alias   => "bc-binary",
            notify  => Service["bcserver"],
            owner   => "bcserver";
        "/usr/bin/APIServer":
            source  => "/binaries/APIServer",
            alias   => "api-binary",
            notify  => Service["apiserver"],
            owner   => "apiserver";
        "/usr/bin/PoolServer":
            source  => "/binaries/PoolServer",
            alias   => "pool-binary",
            notify  => Service["poolserver"],
            owner   => "poolserver";
    }

    if ($test) {
        file {"/usr/bin/MineUserTest":
            ensure => present,
            mode => 0777,
            source => "/binaries/MineUserTest",
            alias => "mineusertest-binary",
        }
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

    exec {"/sbin/setcap 'cap_net_bind_service=+ep' /usr/bin/APIServer":
        refreshonly => true,
        subscribe   => File["api-binary"],
        require => File["api-binary"],
        alias   => "api_bind",
    }

    service {"apiserver":
        require => [
            User["apiserver"],
            Package["zeromq"],
            File["apiserver.conf"],
            File["api-binary"],
            Exec["api_bind"],
        ],
    }

    service {"bcserver":
        require => [
            User["bcserver"],
            Package["zeromq"],
            File["bcserver.conf"],
            File["bc-binary"],
        ],
        subscribe => Service["bitcoind"],
    }

    service {"poolserver":
        require => [
            User["poolserver"],
            Package["zeromq"],
            File["poolserver.conf"],
            File["pool-binary"],
        ],
    }
}
