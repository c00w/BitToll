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

    service {"firehol":
        ensure  => running,
        enable  => true,
        require => [
            File["/etc/firehol/firehol.conf"],
            Package["firehol"]
        ],
        subscribe   => [
            Package["firehol"],
            File["/etc/firehol/firehol.conf"],
        ],
    }
}
