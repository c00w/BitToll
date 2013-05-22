class redis_server {
    apt::ppa {
        'chris-lea':
            alias   => "ppa_redis",
            ensure  => present,
            key     => "C7917B12",
            ppa     => ["redis-server", "zeromq"]
    }

    exec {"apt-get update && touch /var/tmp/redis_apt_update":
        require => Apt::Ppa["ppa_redis"],
        path    => "/usr/bin",
        alias   => "redis_apt_update",
        creates => "/var/tmp/redis_apt_update"
    }

    package {"redis-server":
        require => [
            Apt::Ppa["ppa_redis"],
            Exec["redis_apt_update"],
        ],
        ensure  => latest
    }

    file {"/etc/redis/redis.conf":
        alias   => "redis.conf",
        ensure  => present,
        mode    => 0644,
        owner   => root,
        source  => "/configs/redis.conf",
        require => Package["redis-server"],
        notify  => Service["redis-server"],
    }

    service {"redis-server":
        require => [
            Package["redis-server"],
            File["/etc/redis/redis.conf"],
        ],
        ensure => running,
        enable => true,
        hasstatus => true,
        hasrestart => true,
    }

}
