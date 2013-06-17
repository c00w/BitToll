import "statsd.pp"

class graphite ($test = false) {
    require statsd

    user {"graphite":
        ensure  => present
    }

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
        "python-pip":
            ensure  => latest;
        "python-dev":
            ensure  => latest;
        "python-cairo":
            ensure  => latest;
        "whisper":
            provider    => pip;
        "carbon":
            provider    => pip;
        "graphite-web":
            provider    => pip;
        "python-django":
            ensure      => latest;
        "django-tagging":
            provider    => pip;
    }

    Package["python-dev"] -> Package["carbon"]
    Package["python-pip"] -> Package["whisper"]
    Package["python-pip"] -> Package["graphite-web"]
    Package["python-pip"] -> Package["carbon"]
    Package["python-django"] -> Package["graphite-web"]
    Package["django-tagging"] -> Package["graphite-web"]
    Package["python-cairo"] -> Package["graphite-web"]

    file {"/opt/graphite/conf/carbon.conf":
        require => Package["carbon"],
        ensure  => present,
        mode    => 0444,
        source  => "/configs/carbon/carbon.conf",
        owner   => "graphite",
    }

    file {"/etc/init/carbon.conf":
        require => File["/opt/graphite/conf/carbon.conf"],
        ensure  => present,
        mode    => 0444,
        source  => "/configs/carbon/init/carbon.conf",
    }

    service {"carbon":
        require => [
            File["/etc/init/carbon.conf"],
            File["/opt/graphite/conf/carbon.conf"],
            File["/opt/graphite/conf/storage-schemas.conf"],
            File["/opt/graphite/conf/storage-aggregation.conf"],
        ],
        ensure  => running,
        enable  => true,
    }

    file {"/opt/graphite/conf/storage-schemas.conf":
        require => Package["carbon"],
        ensure  => present,
        mode    => 0444,
        source  => "/configs/carbon/storage-schemas.conf",
        owner   => "graphite",
    }

    file {"/opt/graphite/conf/storage-aggregation.conf":
        require => Package["carbon"],
        ensure  => present,
        mode    => 0444,
        source  => "/configs/carbon/storage-aggregation.conf",
        owner   => "graphite",
    }


    file {"/opt/graphite/conf/graphite.wsgi":
        require => Package["graphite-web"],
        ensure  => present,
        mode    => 0444,
        source  => "/configs/graphite/graphite.wsgi",
        owner   => "graphite",
    }

    file {"/opt/graphite/webapp/graphite/local_settings.py":
        require => Package["graphite-web"],
        ensure  => present,
        mode    => 0444,
        source  => "/configs/graphite/local_settings.py",
        owner   => "graphite",
    }


    exec {"/bin/chown -R graphite /opt/graphite":
        require  => [
            Package["graphite-web"],
            Package["carbon"],
            Package["whisper"],
        ],
        alias   => "graphite_own",
    }

    exec {"/usr/bin/python manage.py syncdb --noinput":
        cwd     => "/opt/graphite/webapp/graphite",
        user    => "graphite",
        creates => "/opt/graphite/storage/graphite.db",
        require => [
            File["/opt/graphite/webapp/graphite/local_settings.py"],
            Exec["graphite_own"],
        ]
    }
}
