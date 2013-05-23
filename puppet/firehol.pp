class firehold {
    package {"firehol":
        ensure  => latest
    }

    file {"/etc/firehol/firehol.conf":
        source  => "/config/firehol.conf",
        mode    => 0600,
        owner   => root,
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
