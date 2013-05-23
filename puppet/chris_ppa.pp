class chris_ppa {
    apt::ppa {"chris-lea":
        ensure  => present,
        key     => "C7917B12",
        ppa     => ["redis-server", "zeromq"]
    }

    exec {"apt-get update && touch /var/tmp/chris_ppa_update":
        require => Apt::Ppa["chris-lea"],
        path    => "/usr/bin",
        creates => "/var/tmp/chris_ppa_update",
    }
}
