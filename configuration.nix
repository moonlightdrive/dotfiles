# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.loader.grub.device = "/dev/sda";

  boot.initrd.luks.devices = [
    {
      name = "root";
      # /dev/sda5
      device = "/dev/disk/by-uuid/5dff34cc-a2f9-49e0-b71a-67f4d70d029c";
      preLVM = true;
    }
  ];

  networking.hostName = "theseus";

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  hardware.pulseaudio.enable = true;

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # $ less ~/.config/plasma-workspace/env/set_window_manager.sh
  # export KDEWM=/run/current-system/sw/bin/xmonad
  nixpkgs.config.packageOverrides = pkgs:
    { xmonad-with-packages = pkgs.xmonad-with-packages.override
      { ghcWithPackages = config.services.xserver.windowManager.xmonad.haskellPackages.ghcWithPackages;
        packages = self: [ self.xmonad-contrib self.xmonad-extras ];
      };
    };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    emacs
    evince
    firefox
    git
    wget

    xmonad-with-packages
  ];

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql100;
    authentication = "local all postgres trust";
   };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services.emacs.enable = true;
  services.emacs.defaultEditor = true;

  services.xserver = {
    enable = true;
    layout = "dvorak";

    videoDrivers = ["intel"];

    # start w systemctl start display-manager
    autorun = true;
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
    desktopManager.default = "plasma5";

    synaptics.enable = true;
    synaptics.twoFingerScroll = true;
  };

users.extraUsers.moonlight = {
    name = "moonlight";
    uid = 1000;
    isNormalUser = true;
    createHome = true;
    group = "users";
    extraGroups = [ "wheel" "audio" ];
    home = "/home/moonlight";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}
