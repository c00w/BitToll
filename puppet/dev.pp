import "build_dep.pp"

node default {
    class {'build_depends':
        deploy_user => $curruser
    }
}
