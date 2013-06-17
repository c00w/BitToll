import "graphite.pp"

class uwsgi ($test = false) {
    require graphite

    if ($test) {
        Package {
            ensure  => present
        }
    } else {
        Package {
            ensure  => latest
        }
    }

    package {
        "uWSGI":
            provider => pip;
        "uwsgi-plugin-python":
            ensure  => absent;
    }

    exec {"/usr/bin/pip install uwsgi":
        creates => "/usr/local/bin/uwsgi"
    }

    file {"/etc/init/uwsgi.conf":
        ensure  => present,
        source  => "/configs/uwsgi/init/uwsgi.conf",
    }

    service {"uwsgi":
        require => [
            Package["uWSGI"],
            Package["uwsgi-plugin-python"],
            File["/etc/init/uwsgi.conf"],
        ],
        ensure  => running,
        enable  => true,
    }
}
