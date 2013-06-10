import "chris_ppa.pp"

class llvm (
    $deploy_user = 'deploy',
    $dir = '/opt/llvm',
    $source = "http://llvm.org/releases/3.2/clang+llvm-3.2-x86_64-linux-ubuntu-12.04.tar.gz",
    $source_name = "clang+llvm-3.2-x86_64-linux-ubuntu-12.04.tar.gz",
    $extract_name = "clang+llvm-3.2-x86_64-linux-ubuntu-12.04",
    ) {

    package {"llvm":
        ensure  => absent
    }

    exec {"wget $source":
        user    => $deploy,
        cwd     => "/home/$deploy",
        path    => "/bin:/usr/bin",
        refreshonly => true,
        creates => "/home/$deploy/$source_name",
        alias   => "llvm_source",
        timeout => 0,
    }

    exec {"tar -xvf /home/$deploy/$source_name":
        cwd     => "/opt/",
        user    => "root",
        creates => "/opt/$extract_name",
        require => Exec["llvm_source"],
        refreshonly => true,
        alias   => "llvm_ex"
    }

    exec {"mv /opt/$extract_name /opt/llvm":
        creates => "/opt/llvm",
        require => Exec["llvm_ex"],
        user    => "user",
        alias   => "llvm_fold",
    }

    exec {"chmod -R 0511 /opt/llvm && chown -R root /opt/llvm":
        user    => "root",
        require => Exec["llvm_fold"],
        refreshonly => true,
    }

    file {"/usr/bin/opt":
        ensure  => link,
        target  => "/opt/llvm/bin/opt",
    }

    file {"/usr/bin/llc":
        ensure  => link,
        target  => "/opt/llvm/bin/llc",
    }

}

class build_depends( $deploy_user = 'deploy') {
    require chris_ppa

    group {"admin":
        ensure  => present,
    }

    if ($deploy_user == "deploy") {
        user {"$deploy_user":
            require => Group["admin"],
            ensure  => present,
            home    => "/home/$deploy_user",
            managehome  => true,
            groups  => ["admin"],
            password    => '$6$/esg1pQd$FLyX3h4azHpMIk4ensRumKA3yOuAH0zIBcH.19.wGvtwA4UHbBcdZcYVqeJAsBTUZznNpzQurMJTbdGfZRnB4/',
            shell       => "/bin/bash",
        }
    } else {
        user {"$deploy_user":
            require => Group["admin"],
            ensure  => present,
            home    => "/home/$deploy_user",
            managehome  => true,
            groups  => ["admin"],
        }
    }

    package { [
            "libgmp3c2",
            "bzip2",
            "libgmp3-dev",
            "alex",
            "libzmq-dev",
            "zlib1g-dev",
            "build-essential",
            "happy",
            "git"]:
        ensure => latest,
    }

    vcsrepo {"/home/$deploy_user/BitToll":
        source  => "git://github.com/c00w/BitToll.git",
        require => [
            User["$deploy_user"],
            Package["git"],
        ],
        ensure  => "present",
        provider=> git,
        user    => "$deploy_user",
    }

    vcsrepo {"/home/$deploy_user/cabal":
        source  => "git://github.com/haskell/cabal.git",
        revision=> "cabal-1.16",
        require => [
            User["$deploy_user"],
            Package["git"],
        ],
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
        timeout => 0,
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
        require => [
            Exec["cabal_lib"],
            Package["zlib1g-dev"],
        ],
        cwd         => "/home/$deploy_user/cabal/cabal-install",
        path        => "/bin:/usr/bin:/usr/local/bin:.",
        alias       => "cabal_install",
        creates     => "/home/$deploy_user/.cabal/bin/cabal",
        environment => "HOME=/home/$deploy_user",
        provider    => "shell",
        user        => "$deploy_user",
        timeout     => 0,
    }

exec {"cabal update && cabal install monadloc-pp":
        require     => Exec["cabal_install"],
        path        => "/bin:/usr/bin:/usr/local/bin:/home/$deploy_user/.cabal/bin/",
        user        => "$deploy_user",
        environment => "HOME=/home/$deploy_user",
        creates     => "/home/$deploy_user/.cabal/bin/MonadLoc",
        alias       => "cabal_monadloc",
        provider    => "shell",
    }

    exec {"cabal update && cabal install cabal-dev":
        require     => Exec["cabal_install"],
        path        => "/bin:/usr/bin:/usr/local/bin:/home/$deploy_user/.cabal/bin/",
        user        => "$deploy_user",
        environment => "HOME=/home/$deploy_user",
        creates     => "/home/$deploy_user/.cabal/bin/cabal-dev",
        alias       => "cabal_dev",
        provider    => "shell",
    }
}
