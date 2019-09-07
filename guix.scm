;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (gnu packages)
             (gnu system nss) 
             (gnu packages version-control) 
             (gnu packages xorg)	
             (gnu packages compression)	
             (gnu packages lxde)	
             (gnu packages web-browsers)	
             (gnu packages fonts)	
             (gnu packages gnuzilla)	
             (nongnu packages linux)
             (gnu packages gcc)	
             (gnu packages mono)	
             (gnu packages ssh)	
             (gnu packages package-management)	
             (gnu packages curl)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages mail)
             (gnu packages terminals)
             (gnu packages xdisorg)
             (gnu packages gnupg)
             (gnu packages readline)
             (gnu packages maths)
             (gnu packages libffi)
             (gnu packages pdf)
             (ice-9 popen)
             (rnrs io ports))

(use-service-modules desktop networking ssh xorg nix)

(use-package-modules certs gnome lisp package-management)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout
  (keyboard-layout "de" "nodeadkeys"))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))
 (swap-devices (list "/dev/sda2"))
 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "FBAE-2F89" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device
           (uuid "6fc7bfbf-5f4c-4d13-9a78-25068705791f"
                 'ext4))
          (type "ext4"))
         %base-file-systems))
 (host-name "klingenberg-tablet")
 (users (cons* (user-account
                (name "klingenberg")
                (comment "Klingenberg")
                (group "users")
                (home-directory "/home/klingenberg")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "nss-certs")
         emacs
         emacs-exwm
         emacs-pdf-tools
         font-adobe-source-code-pro
         mu
         nix
         termite
         pcmanfm
         offlineimap
         sbcl
         gcc
         gsl
         icecat
         mono
         xrandr
         gnupg
         curl
         gvfs
         git)
   %base-packages))
 (services
  (append
   (list (service openssh-service-type)
         (service dhcpd-service-type
                  (dhcpd-configuration
                   (config-file (local-file "/etc/wpa_supplicant.conf"))
                   (interfaces '("wlp1s0"))))
         (service nix-service-type)
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout))))
   %desktop-services)))
