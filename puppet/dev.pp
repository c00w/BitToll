Exec { path => [ "/bin/", "/sbin/" , "/usr/bin/", "/usr/sbin/" ] }

import "build_dep.pp"

node default {
    class {"apt":}
    class {'build_depends':
        deploy_user => $curruser
    }
}
