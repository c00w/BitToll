user {"deploy":
    ensure  => "present",
    home    => "/home/deploy",
    managehome  => true,
    groups  => ["admin"],
}

package { [
        "libgmp3c2",
        "bzip2",
        "libgmp3-dev",
        "build-essential"]:
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

Exec {
    logoutput    => "on_failure",
}

exec {"wget http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-unknown-linux.tar.bz2 && bunzip2 ghc-7.6.3-x86_64-unknown-linux.tar.bz2 -f && tar -xvf ghc-7.6.3-x86_64-unknown-linux.tar":
    user    => "deploy",
    cwd     => "/home/deploy/",
    path    => "/bin:/usr/bin",
    creates => "/home/deploy/ghc-7.6.3",
    alias   => "ghc_source",
    returns => [0, 2],
    require => [
        Package["build-essential"],
        Package["bzip2"],]
}

exec {"configure && make install":
    require => [
        Package["libgmp3c2"],
        Package["libgmp3-dev"],
        Exec["ghc_source"]
    ],
    cwd     => "/home/deploy/ghc-7.6.3",
    path    => "/bin:/usr/bin:/home/deploy/ghc-7.6.3",
    alias   => "ghc_binary",
    creates => "/usr/local/bin/ghc"
}

exec {"ghc --make Setup && Setup configure && Setup build && Setup install":
    require => Exec["ghc_binary"],
    cwd     => "/home/deploy/cabal/Cabal",
    path    => "/bin:/usr/bin:/usr/local/bin:.",
    alias   => "cabal_lib",
    creates => "/home/deploy/cabal/Cabal/Setup"
}

exec {"chmod +x bootstrap.sh && bootstrap.sh":
    require => Exec["cabal_lib"],
    cwd     => "/home/deploy/cabal/cabal-install",
    path    => "/bin:/usr/bin:/usr/local/bin:.",
    alias   => "cabal_install",
    creates => "/home/deploy/.cabal/bin/cabal",
    environment => "HOME=/home/deploy",
    provider    => "shell",
    user    => "deploy",
}

exec {"cabal update && cabal install cabal-dev":
    require => Exec["cabal_install"],
    path    => "/bin:/usr/bin:/usr/local/bin:/home/deploy/.cabal/bin/",
    user    => "deploy",
    environment => "HOME=/home/deploy",
    alias   => "cabal_dev"
}
