;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules
 (srfi srfi-1)
 (gnu)
 (gnu system nss)
 (nongnu packages linux)
 (flat packages emacs)
 (nongnu system linux-initrd)
 (guix packages)
 (guix download)
 (guix utils)
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
 desktop
 pm)

(use-package-modules
 certs
 freedesktop
 gl
 gnome
 pulseaudio
 gcc
 mono
 vpn
 virtualization
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
 lisp-xyz
 wm
 package-management)

                                        ; setup custom sudo rules so some clearly specified commands can be
;; run without password, ALWAYS use absolute filenames here! To
;; continue working when I install a tool as user, I setup sudo-rules
;; for both the system-tools and my user-tools.
(define %sudoers-specification
  (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
%wheel ALL=(ALL) NOPASSWD: /run/current-system/profile/sbin/shutdown,\
 /run/current-system/profile/sbin/reboot,\
 /run/current-system/profile/bin/light,\
 /run/current-system/profile/bin/mount,\
 /run/current-system/profile/bin/umount"))


(operating-system
 (locale "en_US.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout
  (keyboard-layout "de"
                   "nodeadkeys"
                   #:options
                   '("ctrl:nocaps")))
 (host-name "klingenberg-laptop")
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
 (sudoers-file %sudoers-specification)
 (packages
  (append
   (list
    (specification->package "nss-certs")
    ;; xf86-video-intel
    intel-vaapi-driver
    mesa
    mesa-opencl
    mesa-utils
    light
    sbcl
    sbcl-slynk
    stumpwm
    `(,stumpwm "lib")
    stumpish
    sbcl-stumpwm-ttf-fonts
    font-dejavu
    font-adobe-source-code-pro
    font-fira-code
    font-fira-mono
    font-fira-sans
    xdg-utils
    sbcl-stumpwm-pass
    sbcl-stumpwm-wifi
    sbcl-stumpwm-stumptray
    emacs-stumpwm-mode
    ;; emacs-native-comp
    emacs
    ;; emacs-exwm
    emacs-guix
    ;; emacs-pdf-tools
    ;; emacs-pulseaudio-control
    emacs-vterm
    guile-gcrypt
    acpi
    mu
    isync
    openssl
    qemu
    zip
    unzip
    nix
    password-store
    ;; libffi
    ;; guile-3.0-latest
    gcc
    gcc-toolchain
    arandr
    xrandr
    pinentry-emacs
    ;; pinentry
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
    (service openssh-service-type)
    (service slim-service-type
             (slim-configuration
              (display ":1")
              (vt "vt7")
              (auto-login? #t)
              (default-user "klingenberg")
              (xorg-configuration
               (xorg-configuration
                (keyboard-layout keyboard-layout)))))
    (service nix-service-type)
    (service thermald-service-type)
    (service openvpn-client-service-type
             (openvpn-client-configuration))
    (bluetooth-service
     #:auto-enable? #t))
   (remove (lambda (service)
             (eq? (service-kind service) gdm-service-type))
           %desktop-services)
   ;; %desktop-services
   ))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (uuid "1865ada0-a67d-47ac-b4fa-8403981404c9")))
 (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "F62F-24CC" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "1a912f56-30f5-4eeb-b275-456446dfd5af"
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
  (specification->package "linux")
  ;; (specification->package "linux-libre")
  ;; (let*
  ;;     ((channels
  ;;       (list
  ;;        (channel
  ;;         (name 'nonguix)
  ;;         (url "https://gitlab.com/nonguix/nonguix")
  ;;         (commit "0a5cd133a3097b192702c8b806f2a6f54182118d"))
  ;;        (channel
  ;;         (name 'flat)
  ;;         (url "https://github.com/flatwhatson/guix-channel.git")
  ;;         (commit "c93cbe457c23709350a6e432e0e196263eb4fc08"))
  ;;        (channel
  ;;         (name 'guix)
  ;;         (url "https://git.savannah.gnu.org/git/guix.git")
  ;;         (commit "37d8def701839200c44c76cb2fa2abfb27a7b88b"))))
  ;;      (inferior
  ;;       (inferior-for-channels channels)))
  ;;   (first (lookup-inferior-packages inferior "linux" "5.11.10")))
  )
 ;; (kernel-arguments
;;   (append '(

;; "i915" "enable_dc=2"

;; "i915" "disable_power_well=0"

;; "i915" "enable_fbc=1"

;; "i915" "enable_guc=3"

;; "i915" "enable_dpcd_backlight=1"

;;             )
;;          %default-kernel-arguments))
 (initrd microcode-initrd)
 (firmware
  (list linux-firmware)))
