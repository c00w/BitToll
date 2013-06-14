import "chris_ppa.pp"
import "git.pp"

class nodejs {

    require chris_ppa
    require git

    package{"nodejs":
        ensure  => latest
    }
}

class statsd {
    require git
    require nodejs

    user{"statsd":
        ensure  => present,
        home    => "/home/statsd",
        managehome  => true,
        password    => '*',
    }

    vcsrepo {"/home/statsd/statsd":
        require => User["statsd"],
        source  => 'git://github.com/etsy/statsd.git',
        user    => "statsd",
        ensure  => "present",
        revision    => "v0.6.0",
        provider    => git,
    }

    file {"/etc/init/statsd.conf":
        ensure  => present,
        mode    => 0644,
        source  => "/configs/statsd.conf"
    }

    file {"/home/statsd/Config.js":
        require => User["statsd"],
        source  => "/configs/statsd/Config.js",
        owner   => "statsd",
        mode    => 0644,
    }

    service {"statsd":
        require => [
            File["/etc/init/statsd.conf"],
            Vcsrepo["/home/statsd/statsd"]
        ],
        ensure  => running,
        enable  => true,
    }
}
