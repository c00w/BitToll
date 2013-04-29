
node default {
    class {'ntp':}
    import "test_vm.pp"
}

