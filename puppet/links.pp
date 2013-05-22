import "build.pp"

class links ($user = 'deploy') {

    require build

    file {"/configs":
        ensure  => "link",
        target  => "/home/$user/BitToll/configs",
    }

    file {"/binaries":
        ensure  => "link",
        target  => "/home/$user/BitToll/cabal-dev/bin",
    }

}
