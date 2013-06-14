import "git.pp"

class chris_ppa {
    require git

    apt::ppa {"chris-lea":
        ensure  => present,
        key     => "C7917B12",
        ppa     => ["redis-server", "zeromq", "node.js"]
    }

    exec {"/usr/bin/apt-get update":
        require => Apt::Ppa["chris-lea"],
        subscribe => Apt::Ppa["chris-lea"],
        refreshonly => true,
    }
}
