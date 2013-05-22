import "chris_ppa.pp"

class build_depends( $deploy_user = 'deploy') {
    require chris_ppa

    group {"admin":
        ensure  => present,
    }

    user {"$deploy_user":
        require => Group["admin"],
        ensure  => present,
        home    => "/home/$deploy_user",
        managehome  => true,
        groups  => ["admin"],
    }

    package { [
            "libgmp3c2",
            "bzip2",
            "libgmp3-dev",
            "alex",
            "libzmq-dev",
            "build-essential"]:
        ensure => latest,
    }

    vcsrepo {"/home/$deploy_user/BitToll":
        source  => "git://github.com/c00w/BitToll.git",
        revision=> "production",
        require => User["$deploy_user"],
        ensure  => "present",
        provider=> git,
        user    => "$deploy_user",
    }

    vcsrepo {"/home/$deploy_user/cabal":
        source  => "git://github.com/haskell/cabal.git",
        revision=> "cabal-1.16",
        require => User["$deploy_user"],
        ensure  => "present",
        provider=> git,
        user    => "$deploy_user",
    }

    Exec {
        logoutput    => "on_failure",
    }

    exec {"wget http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-unknown-linux.tar.bz2 && bunzip2 ghc-7.6.3-x86_64-unknown-linux.tar.bz2 -f && tar -xvf ghc-7.6.3-x86_64-unknown-linux.tar":
        user    => "$deploy_user",
        cwd     => "/home/$deploy_user/",
        path    => "/bin:/usr/bin",
        creates => "/home/$deploy_user/ghc-7.6.3",
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
        cwd     => "/home/$deploy_user/ghc-7.6.3",
        path    => "/bin:/usr/bin:/home/$deploy_user/ghc-7.6.3",
        alias   => "ghc_binary",
        creates => "/usr/local/bin/ghc"
    }

    exec {"ghc --make Setup && Setup configure && Setup build && Setup install":
        require => Exec["ghc_binary"],
        cwd     => "/home/$deploy_user/cabal/Cabal",
        path    => "/bin:/usr/bin:/usr/local/bin:.",
        alias   => "cabal_lib",
        creates => "/home/$deploy_user/cabal/Cabal/Setup"
    }

    exec {"chmod +x bootstrap.sh && bootstrap.sh":
        require => Exec["cabal_lib"],
        cwd     => "/home/$deploy_user/cabal/cabal-install",
        path    => "/bin:/usr/bin:/usr/local/bin:.",
        alias   => "cabal_install",
        creates => "/home/$deploy_user/.cabal/bin/cabal",
        environment => "HOME=/home/$deploy_user",
        provider    => "shell",
        user    => "$deploy_user",
    }

    exec {"cabal update && cabal install cabal-dev":
        require => Exec["cabal_install"],
        path    => "/bin:/usr/bin:/usr/local/bin:/home/$deploy_user/.cabal/bin/",
        user    => "$deploy_user",
        environment => "HOME=/home/$deploy_user",
        creates => "/home/$deploy_user/.cabal/bin/cabal-dev",
        alias   => "cabal_dev"
    }
}
