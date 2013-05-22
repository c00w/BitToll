import "redis.pp"
import "bitcoind.pp"
import "bittoll.pp"
import "p2pool.pp"

class {"redis_server":}
class {"bitcoind":
    test    => true
}
class {"p2pool":
    test    => true
}


class {"bittoll":
    test    => true
}

$test_packages = [
    "git",
    "vim",
    "htop",
]

package { $test_packages:
    ensure  =>  latest,
}

file {"/home/vagrant/.bitcoin":
    ensure => link,
    target => "/home/bitcoind/.bitcoin",
    mode => 0644,
    owner => "vagrant",
}

