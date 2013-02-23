
$test_packages = [
    "ghc",
    "cabal-install",
    "htop",
    "zsh",
    "git",
    "alex",
    "g++",
    "make",
    "redis-server",
    "bitcoind",
]

package { $test_packages:
    ensure  =>  latest,
}

exec { "git clone git://github.com/c00w/BitToll.git":
    alias   => "gitcloned",
    cwd     =>  "/home/ubuntu/",
    creates =>  "/home/ubuntu/BitToll",
    require =>  Package['git'],
    path    =>  ["/usr/bin"],
    user    => "ubuntu",
}

exec { "cabal update":
    alias   =>  "cabal_update",
    path    =>  ["/usr/bin"],
    user    =>  "ubuntu",
    require =>  [
        Package["cabal-install"],
        Package["ghc"],
        ],
}

exec { "cabal install":
    cwd     =>  "/home/ubuntu/BitToll",
    creates =>  [
        "/home/ubuntu/.cabal/bin/APIServer",
        "/home/ubuntu/.cabal/bin/BTServer",
        ],
    require =>  [
        Package["cabal-install"],
        Package["ghc"],
        Package["alex"],
        Exec["gitcloned"],
        Exec["cabal_update"],
        ],
    path    =>  ["/usr/bin"],
    user    => "ubuntu",
    timeout => 0,
}
