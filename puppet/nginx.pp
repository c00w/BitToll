
class nginx ($test = false) {

    apt::ppa {"nginx-openresty":
        ensure  => present,
        key     => "39AB0BFD",
        ppa     => "stable",
    }

    exec {"/usr/bin/apt-get update && echo":
        require     => Apt::Ppa["nginx-openresty"],
        subscribe   => Apt::Ppa["nginx-openresty"],
        refreshonly => true,
    }

    package {"nginx-openresty":
        ensure  => latest,
        require  => Apt::Ppa["nginx-openresty"],
    }

    $config = $test ? {
        true    => "/configs/test",
        false   => "/configs",
    }

    file {"/etc/nginx/sites-available/default":
        notify  => Service["nginx"],
        require => Package["nginx-openresty"],
        ensure  => absent,
    }

    file{"/etc/nginx/sites-available/default.dpkg-dist":
        notify  => Service["nginx"],
        require => Package["nginx-openresty"],
        ensure  => absent,
    }

    file {"/etc/nginx/sites-enabled/default":
        notify  => Service["nginx"],
        require => Package["nginx-openresty"],
        ensure  => absent
    }

    file {"/etc/nginx/sites-available/bittoll.conf":
        notify  => Service["nginx"],
        require => Package["nginx-openresty"],
        ensure  => present,
        mode    => 0400,
        source  => "$config/nginx/bittoll.conf",
    }

    file {"/etc/nginx/sites-enabled/bittoll.conf":
        notify  => Service["nginx"],
        require => Package["nginx-openresty"],
        ensure  => link,
        target  => "/etc/nginx/sites-available/bittoll.conf",
    }

    file {"/etc/nginx/nginx.conf":
        notify  => Service["nginx"],
        require => Package["nginx-openresty"],
        ensure  => file,
        mode    => 0400,
        source  => "$config/nginx.conf",
    }

    service {"nginx":
        require => Package["nginx-openresty"],
        ensure  => running,
        enable  => true,
        hasrestart  => true,
    }

    file {"/usr/share/nginx/html/":
        ensure  => directory,
        recurse => true,
        purge   => true,
        force   => true,
        owner   => "www-data",
        group   => "www-data",
        mode    => 0444,
        source  => "/configs/web_root",
    }
}
