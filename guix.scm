;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules
 (srfi srfi-1)
 (gnu)
 (gnu system nss)
 (nongnu packages linux)
 (nongnu packages linux)
 (nongnu system linux-initrd)
 (guix packages)
 (guix channels)
 (guix inferior)
 (ice-9 popen)
 (rnrs io ports))

(use-service-modules
 networking
 sound
 vpn
 ssh
 nix
 xorg
 desktop)

(use-package-modules
 certs
 gnome
 pulseaudio
 gcc
 mono
 vpn
 tls
 sqlite
 version-control
 xorg
 linux
 compression
 video
 lxde
 display-managers
 guile
 commencement
 web-browsers
 password-utils
 fonts
 syncthing
 gnuzilla
 ssh
 package-management
 curl
 emacs
 emacs-xyz
 mail
 terminals
 xdisorg
 gnupg
 mate
 readline
 tex
 maths
 libffi
 pdf
 lisp
 package-management)

(operating-system
 (locale "en_US.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout
  (keyboard-layout "de"
                   "nodeadkeys"
                   #:options
                   '("ctrl:nocaps"))
  )
 (host-name "klingenberg-tablet")
 (users
  (cons*
   (user-account
    (name "klingenberg")
    (comment "Klingenberg")
    (group "users")
    (home-directory "/home/klingenberg")
    (supplementary-groups
     '("wheel" "netdev" "audio" "video" "lp" "dialout")))
   %base-user-accounts))
 (packages
  (append
   (list
    (specification->package "nss-certs")
    emacs
    emacs-exwm
    emacs-guix
    ;; emacs-pdf-tools
    guile-gcrypt
    emacs-pulseaudio-control
    font-adobe-source-code-pro
    acpi
    mu
    isync
    openssl
    zip
    unzip
    nix
    password-store
    sbcl
    ;; libffi
    ;; guile-3.0-latest
    gcc
    gcc-toolchain
    arandr
    xrandr
    pinentry-emacs
    gnupg
    openvpn
    sqlite
    mate-icon-theme-faenza
    curl
    gvfs
    xinput
    git)
   %base-packages))
 (services
  (append
   (list
    ;; (service xfce-desktop-service-type)
    (service slim-service-type
             (slim-configuration
              (display ":1")
              (vt "vt7")
              (auto-login? #t)
              (default-user "klingenberg")
              (xorg-configuration
               (xorg-configuration
                (keyboard-layout keyboard-layout)))
              ))
    (service nix-service-type)
    (service openvpn-client-service-type
             (openvpn-client-configuration))
    (bluetooth-service
     #:auto-enable? #t))
   (remove (lambda (service)
             (eq? (service-kind service) gdm-service-type))
           %desktop-services)))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)
   ))
 (swap-devices
  (list "/dev/sda2"))
 (file-systems
  (cons*
   (file-system
    (mount-point "/boot/efi")
    (device
     (uuid "CEFA-D25C" 'fat32))
    (type "vfat"))
   (file-system
    (mount-point "/")
    (device
     (uuid "de52c9b8-e250-4707-807d-38f66bef1383"
           'ext4))
    (type "ext4"))
   %base-file-systems))
 (setuid-programs
  (append
   (list
    "/run/current-system/profile/sbin/shutdown"
    "/run/current-sytem/profile/sbin/reboot")
   %setuid-programs))
 (kernel
  (let*
      ((channels
        (list (channel
               (name 'nonguix)
               (url "https://gitlab.com/nonguix/nonguix")
               (commit "277c59d26f31acbcb7c282e760491b9251583986"))
              (channel
               (name 'guix)
               (url "https://git.savannah.gnu.org/git/guix.git")
               (commit "0e7731ba801a581a00fdf6113c87c46236c673c5"))))
       (inferior
        (inferior-for-channels channels)))
    (car (lookup-inferior-packages inferior "linux" "5.8.5")))
  ;; linux
  )
 (initrd microcode-initrd)
 (firmware
  (list linux-firmware)))
