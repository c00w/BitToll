import "build_dep.pp"

class build ($user = 'deploy') {
    require build_depends

    exec {"cabal-dev install":
        refreshonly => true,
        subscribe   => Class["build_depends"],
        require     => [
            Package["alex"],
            Package["libzmq-dev"],
        ],
        cwd         => "/home/$user/BitToll",
        path        => "/bin:/usr/bin:/usr/local/bin:/home/$user/.cabal/bin",
        user        => "$user",
        environment => "HOME=/home/$user",
        provider    => "shell",
        logoutput   => "on_failure",
    }
}
