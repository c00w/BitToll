# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "precise64"
  config.vm.hostname = "test.m.bittoll.com"
  config.vm.box_url = "http://files.vagrantup.com/precise64.box"

  #p2pool
  #config.vm.network :forwarded_port, guest: 19332, host: 19332
  #testnet-box
  #config.vm.network :forwarded_port, guest: 19001, host: 19001

  config.vm.network :private_network, ip: "192.168.56.2"

  config.vm.synced_folder "configs", "/configs"
  config.vm.synced_folder "cabal-dev/bin", "/binaries"

  config.vm.provider :virtualbox do |vb|
      vb.customize [
        "modifyvm", :id,
        "--memory", "2048",
        "--cpus", "4"]
  end

  config.vm.provision :puppet do |puppet|
     puppet.manifests_path = "puppet"
     puppet.manifest_file  = "base.pp"
     puppet.module_path    = "puppet_modules"
     puppet.options        = ""
  end
end
