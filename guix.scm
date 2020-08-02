;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (gnu packages vpn)
             (gnu services vpn)
             (gnu services sound)
             (gnu system nss)
             (gnu packages version-control)
             (gnu packages xorg)
             (gnu packages linux)
             (gnu packages compression)
             (gnu packages video)
             (gnu packages lxde)
             (gnu packages web-browsers)
             (gnu packages password-utils)
             (gnu packages fonts)
             (gnu packages display-managers)
             (gnu packages syncthing)
             (gnu packages gnuzilla)
             (nongnu packages linux)
             (gnu packages gcc)
             (gnu packages mono)
             (gnu packages ssh)
             (gnu packages package-management)
             (gnu packages curl)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (emacs-exwm-next)
             (gnu packages pulseaudio)
             (gnu packages mail)
             (gnu packages terminals)
             (gnu packages xdisorg)
             (gnu packages gnupg)
             (gnu packages gnome)
             (gnu packages mate)
             (gnu packages readline)
             (gnu packages tex)
             (gnu packages maths)
             (gnu packages libffi)
             (gnu packages pdf)
             (ice-9 popen)
             (rnrs io ports)
             (gnu packages emacs)
             (gnu packages version-control))

(use-service-modules desktop networking ssh xorg nix)

(use-package-modules certs gnome lisp package-management)

(operating-system
 (locale "en_US.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout
  (keyboard-layout "de"
                   "nodeadkeys"
                   #:options '("ctrl:nocaps")))
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
         emacs-next
         emacs-guix
         emacs-exwm-next
         emacs-pdf-tools
         emacs-pulseaudio-control
         font-adobe-source-code-pro
         acpi
         mu
         zip
         unzip
         nix
         password-store
         sbcl
         gcc
         ;; gsl
         arandr
         xrandr
         pinentry-emacs
         gnupg
         openvpn
         mate-icon-theme-faenza
         curl
         gvfs
         git)
   %base-packages))
 (services
  (append
   (list (service xfce-desktop-service-type)
         (service nix-service-type)
         (service openvpn-client-service-type
                  (openvpn-client-configuration))
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout))))
   %desktop-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))
 (swap-devices (list "/dev/sda2"))
 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "CEFA-D25C" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device
           (uuid "de52c9b8-e250-4707-807d-38f66bef1383"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
