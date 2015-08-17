sudo groupadd nixbld
sudo usermod -a -G nixbld vagrant
sudo mkdir /nix
sudo chown -R vagrant /nix
sudo -u vagrant curl https://nixos.org/nix/install | sh
