import "chris_ppa"

class config ($test = false) {

    file {"/etc/bittoll":
        ensure  => directory,
        mode    => 0444,
        owner   => "root"
    }

    $source = $test ? {
        true    => "/configs/test/bittoll.conf",
        false   => "/configs/bittoll.conf"
    }

    file {"/etc/bittoll/bittoll.conf":
        ensure  => present,
        mode    => 0444,
        owner   => "root",
        source  => $source,
        require => File["/etc/bittoll"]
    }

    file {"/usr/bin/ReadConfig":
        ensure  => present,
        source  => "/binaries/ReadConfig",
        mode    => 0111,
        owner   => "root",
    }
}
