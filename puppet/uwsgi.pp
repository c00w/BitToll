import "graphite.pp"

class uwsgi {
    require graphite

    package {
        "uwsgi":
            ensure  => latest,
            provider => pip;
        "uwsgi-plugin-python":
            ensure  => latest;
    }

    file {"/etc/init/uwsgi.conf":
        ensure  => present,
        source  => "/configs/uwsgi/init/uwsgi.conf",
    }

    service {"uwsgi":
        require => [
            Package["uwsgi"],
            Package["uwsgi-plugin-python"],
            File["/etc/init/uwsgi.conf"],
        ],
        ensure  => running,
        enable  => true,
    }
}
