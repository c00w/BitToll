user {"deploy":
    ensure  => "present",
    home    => "/home/deploy",
    managehome  => true,
    groups  => ["admin"],
}

package { ["libgmp3c2", "build-essential"]:
    ensure => latest,
}

vcsrepo {"/home/deploy/BitToll":
    source  => "git://github.com/c00w/BitToll.git",
    revision=> "production",
    require => User["deploy"],
    ensure  => "present",
    provider=> git,
    user    => "deploy",
}


vcsrepo {"/home/deploy/cabal":
    source  => "git://github.com/haskell/cabal.git",
    revision=> "cabal-1.16",
    require => User["deploy"],
    ensure  => "present",
    provider=> git,
    user    => "deploy",
}

exec {"wget http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-unknown-linux.tar.bz2 && bunzip2 ghc-7.6.3-x86_64-unknown-linux.tar.bz2 -f && tar -xvf ghc-7.6.3-x86_64-unknown-linux.tar":
    user    => "deploy",
    cwd     => "/home/deploy/",
    path    => "/usr/bin",
    creates => "/home/deploy/ghc",
    alias   => "ghc_source",
    require => Package["build-essential"],
}

exec {"make install":
    require => [
        Package["libgmp3c2"],
        Exec["ghc_source"]
    ],
    cwd     => "/home/deploy/ghc",
    path    => "/usr/bin",
    alias   => "ghc_binary",
    creates => "/usr/bin/ghc"
}

exec {"ghc --make Setup && ./Setup configure && ./Setup build && ./Setup install":
    cwd     => "/home/deploy/cabal/Cabal",
    path    => "/usr/bin",
    alias   => "cabal_lib",
}

