import "git.pp"

class test_vm {
    require git
    $test_packages = [
        "vim",
        "htop",
    ]

    package { $test_packages:
        ensure  =>  latest,
    }

    file {"/home/vagrant/.bitcoin":
        ensure  => link,
        target  => "/home/bitcoind/.bitcoin",
        mode    => 0644,
        owner   => "vagrant",
    }
}
