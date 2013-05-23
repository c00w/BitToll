class firehol {
    package {"firehol":
        ensure  => latest
    }

    file {"/etc/firehol/firehol.conf":
        source  => "/configs/firehol.conf",
        mode    => 0600,
        owner   => root,
        group   => root,
        ensure  => present,
    }

    file {"/etc/default/firehol":
        source  => "/configs/firehol",
        mode    => 0600,
        owner   => root,
        group   => root,
        ensure  => present,
    }


    service {"firehol":
        ensure  => running,
        enable  => true,
        require => [
            File["/etc/firehol/firehol.conf"],
            File["/etc/default/firehol"],
            Package["firehol"]
        ],
        subscribe   => [
            Package["firehol"],
            File["/etc/firehol/firehol.conf"],
        ],
    }
}
