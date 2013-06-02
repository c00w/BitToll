
class nginx ($test = false) {
    package {"nginx":
        ensure  => latest
    }

    File {
        require => Package["nginx"],
        notify  => Service["nginx"],
        mode    => 0400,
    }

    $config = $test ? {
        true    => "/configs/test",
        false   => "/configs",
    }

    file {"/etc/nginx/sites-available/default":
        ensure  => absent,
    }

    file {"/etc/nginx/sites-enabled/default":
        ensure  => absent
    }

    file    {"/etc/nginx/sites-available/bittoll.conf":
        ensure  => present,
        source  => "$config/nginx/bittoll.conf",
    }

    file {"/etc/nginx/sites-enabled/bittoll.conf":
        ensure  => link,
        target  => "/etc/nginx/sites-available/bittoll.conf",
    }

    file {"/etc/nginx/nginx.conf":
        ensure  => file,
        source  => "$config/nginx.conf",
    }

    service {"nginx":
        require => Package["nginx"],
        ensure  => running,
        enable  => true,
        hasrestart  => true,
    }
}
