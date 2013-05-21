
node default {
    import "build.pp"
    class {'build_depends':
        deploy_user => $curruser
    }
}
