import "chris_ppa"

class redis_server {
    require chris_ppa

    package {"redis-server":
        ensure  => "2:2.6.13-1chl1~precise1"
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
