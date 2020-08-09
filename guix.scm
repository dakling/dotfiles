;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules
 (srfi srfi-1)
 (gnu)
 (gnu packages vpn)
 (gnu system nss)
 (gnu packages tls)
 (gnu packages sqlite)
 (gnu packages version-control)
 (gnu packages xorg)
 (gnu packages linux)
 (gnu packages compression)
 (gnu packages video)
 (gnu packages lxde)
 (gnu packages display-managers)
 (gnu packages guile)
 (gnu packages commencement)
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
 (nongnu packages linux)
 (nongnu system linux-initrd)
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
 (guix packages)
 (guix channels)
 (guix inferior)
 (ice-9 popen)
 (rnrs io ports))

(use-service-modules networking sound vpn ssh nix xorg desktop)

(use-package-modules certs gnome lisp package-management)

(operating-system
 (locale "en_US.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout
  (keyboard-layout "de"
                   "nodeadkeys"
                   #:options
                   '("ctrl:nocaps")))
 (host-name "klingenberg-tablet")
 (users
  (cons*
   (user-account
    (name "klingenberg")
    (comment "Klingenberg")
    (group "users")
    (home-directory "/home/klingenberg")
    (supplementary-groups
     '("wheel" "netdev" "audio" "video" "lp")))
   %base-user-accounts))
 (packages
  (append
   (list
    (specification->package "nss-certs")
    emacs-next
    (package
     (inherit emacs-exwm)
     (name "emacs-exwm-next")
     (synopsis "Emacs X window manager using emacs-next")
     (arguments
      `(#:emacs ,emacs-next))
     (home-page "https://github.com/ch11ng/exwm")
     (description
      "EXWM is a full-featured tiling X window manager for Emacs built on top
of XELB.")
     (arguments
      `(#:emacs ,emacs-next
        #:phases
        (modify-phases %standard-phases
                       (add-after 'build 'install-xsession
               (lambda*
                                   (#:key inputs outputs #:allow-other-keys)
                                   (let*
                                       ((out
                                         (assoc-ref outputs "out"))
                                        (xsessions
                                         (string-append out "/share/xsessions"))
                                        (bin
                                         (string-append out "/bin"))
                                        (exwm-executable
                                         (string-append bin "/exwm")))
                                     ;; Add a .desktop file to xsessions
                                     (mkdir-p xsessions)
                                     (mkdir-p bin)
                                     (make-desktop-entry-file
                                      (string-append xsessions "/exwm.desktop")
                                      #:name ,name
                                      #:comment ,synopsis
                                      #:exec exwm-executable
                                      #:try-exec exwm-executable)
                                     ;; Add a shell wrapper to bin
                                     (with-output-to-file exwm-executable
                                       ;; (lambda _
                     ;;                     (format #t "#!~a ~@
                     ;; ~a +SI:localuser:$USER ~@
                     ;; exec ~a --exit-with-session ~a \"$@\" --eval '~s' ~%"
                     ;;                             (string-append
                     ;;                              (assoc-ref inputs "bash")
                     ;;                              "/bin/sh")
                     ;;                             (string-append
                     ;;                              (assoc-ref inputs "xhost")
                     ;;                              "/bin/xhost")
                     ;;                             (string-append
                     ;;                              (assoc-ref inputs "dbus")
                     ;;                              "/bin/dbus-launch")
                     ;;                             (string-append
                     ;;                              (assoc-ref inputs "emacs")
                     ;;                              "/bin/emacs")
                     ;;                             '(cond
                     ;;                               ((file-exists-p "~/.exwm")
                     ;;                                (load-file "~/.exwm"))
                     ;;                               ((not
                     ;;                                 (featurep 'exwm))
                     ;;                                (require 'exwm)
                     ;;                                (require 'exwm-config)
                     ;;                                (exwm-config-default)
                     ;;                                (message
                     ;;                                 (concat "exwm configuration not found. "
                     ;;                                         "Falling back to default configuration..."))))))
                                       (lambda _
                                         (format #t "#!~a ~@
                     ~a +SI:localuser:$USER ~@
                     source /home/klingenberg/.xprofile ~@
                     exec ~a"
                                                 (string-append
                                                  (assoc-ref inputs "bash")
                                                  "/bin/bash")
                                                 (string-append
                                                  (assoc-ref inputs "xhost")
                                                  "/bin/xhost")
                                                 (string-append
                                                  (assoc-ref inputs "emacs")
                                                  "/bin/emacs")
                                                 '(cond
                                                   ((file-exists-p "~/.exwm")
                                                    (load-file "~/.exwm"))
                                                   ((not
                                                     (featurep 'exwm))
                                                    (require 'exwm)
                                                    (require 'exwm-config)
                                                    (exwm-config-default)
                                                    (message
                                                     (concat "exwm configuration not found. "
                                                             "Falling back to default configuration...")))))))
                                     (chmod exwm-executable #o555)
                                     #t)))))))
    emacs-guix
    emacs-geiser
    guile-gcrypt
    emacs-pdf-tools
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
                (keyboard-layout keyboard-layout)))))
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
   (keyboard-layout keyboard-layout)))
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
  ;; TODO figure out commits
  (let*
      ((channels
        (list (channel
               (name 'nonguix)
               (url "https://gitlab.com/nonguix/nonguix")
               (commit "1101d8070d80a8a4fd40a7f6f4e540bf56cbf6cd"))
              (channel
               (name 'guix)
               (url "https://git.savannah.gnu.org/git/guix.git")
               (commit "2d80caa86c5450938ec859fcab593bff9e4ceef4"))))
       (inferior
        (inferior-for-channels channels)))
    (car (lookup-inferior-packages inferior "linux" "5.7.14")))
  ;; linux
  )
 (initrd microcode-initrd)
 (firmware
  (list linux-firmware)))
