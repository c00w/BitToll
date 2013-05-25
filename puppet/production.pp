import "build_dep.pp"

class production {

    require build_depends

    $packages = [
        "git",
        "vim",
        "htop",
        "iotop",
    ]

    package { $packages:
        ensure  =>  latest,
    }

    ssh_authorized_key {"Colin_Rice_16384_rsa_deploy":
        ensure  => present,
        key     =>"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",
        user    => "deploy",
        type    => "ssh-rsa",
    }

    user {'root':
        ensure      => present,
        password    => '*',
    }

    file {'/etc/ssh/sshd_config':
        ensure  => present,
        owner   => root,
        group   => root,
        source  => '/configs/sshd_config',
        mode    => 0600,
        notify  => Service['ssh']
    }

    service {'ssh':
        ensure  => running,
        enable  => true,
    }

    file {"/etc/cron.daily/bitcoind":
        ensure  => present,
        owner   => root,
        group   => root,
        content => "#!/bin/bash\nrestart bitcoind",
        mode    => 0755,
    }
}
