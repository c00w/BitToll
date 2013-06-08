
class test_vm {
    $test_packages = [
        "vim",
        "htop",
        "git",
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
