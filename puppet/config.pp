import "chris_ppa"

class config ($test = false) {

    File {
        owner   => "root",
        group   => "root",
    }

    file {"/etc/bittoll":
        ensure  => directory,
        mode    => 0444,
    }

    $source = $test ? {
        true    => "/configs/test/bittoll/bittoll.conf",
        false   => "/configs/bittoll/bittoll.conf"
    }

    file {"/etc/bittoll/credit.html":
        ensure  => present,
        mode    => 0444,
        source  => "/configs/bittoll/credit.html",
        require => File["/etc/bittoll"]
    }

    file {"/etc/bittoll/bittoll.conf":
        ensure  => present,
        mode    => 0444,
        source  => $source,
        require => File["/etc/bittoll"]
    }

    file {"/usr/bin/ReadConfig":
        ensure  => present,
        source  => "/binaries/ReadConfig",
        mode    => 0111,
    }
}
