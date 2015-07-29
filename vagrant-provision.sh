sudo groupadd nixbld
sudo usermod -a -G nixbld vagrant
sudo -u vagrant curl https://nixos.org/nix/install | sh
sudo chown -R vagrant /nix
