;; This is an operating system configuration template
;; for a "desktop" setup with GNOME and Xfce where the
;; root partition is encrypted with LUKS.

(use-modules 
	(gnu) 
  (gnu packages)
	(gnu system nss) 
	(gnu packages version-control) 
  (gnu packages xorg)	
  (gnu packages compression)	
  (gnu packages web-browsers)	
  (gnu packages gnuzilla)	
  (chromium chromium)
  (gnu packages gcc)	
  (gnu packages ssh)	
  (gnu packages package-management)	
  (gnu packages curl)
  (gnu packages emacs)
  (gnu packages emacs-xyz)
  (gnu packages terminals)
  (gnu packages xdisorg)
  (gnu packages gnupg)
  (gnu packages readline)
  ;; (gnu packages python)
  ;; (gnu packages python-xyz)
  (gnu packages maths)
  (gnu packages libffi)
  (gnu packages pdf)
  (ice-9 popen)
  (rnrs io ports))

(use-service-modules desktop networking ssh)

(use-package-modules certs gnome lisp)

(operating-system
  (host-name "klingenbergLaptop")
  (timezone "Europe/Paris")
  (locale "en_US.utf8")
;  (firmware (append (list linux-firmware-iwlwifi)
;		                %base-firmware)
;)

  ;; Use the lecay
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/sda")))


  ;; Specify a mapped device for the encrypted root partition.
  ;; The UUID is that returned by 'cryptsetup luksUUID'.
  (mapped-devices
   (list (mapped-device
          (source (uuid "f721379f-7a73-4151-b719-043aac9c91a9"))
          (target "my-root")
          (type luks-device-mapping))))

  (file-systems (cons (file-system
                        (device (file-system-label "my-root"))
                        (mount-point "/")
                        (type "ext4")
                        (dependencies mapped-devices))
                      %base-file-systems))

  (users (cons (user-account
                (name "klingenberg")
                (comment "klingenberg")
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video"))
                (home-directory "/home/klingenberg"))
               %base-user-accounts))

  ;; This is where we specify system-wide packages.
  (packages (cons*
             nss-certs         ;for HTTPS access
             gvfs              ;for user mounts
             gcc
             curl
	     openssh
             rlwrap
             gnupg
             emacs
             termite
             git
             rofi
             sbcl
             sbcl-cffi-libffi
             emacs-exwm
             sbcl-next
             ;; python python2-matplotlib
             gsl
             libffi
             setxkbmap
             unzip
             icecat
	     chromium
	     flatpak
             zathura-pdf-poppler zathura
             %base-packages))

  ;; (operating-system
  ;;  (host-name "...")
  ;;  ;;...

  ;;  (kernel (cond
	;;           ((string-match "Network controller: Intel Corporation Wireless 8888"
	;; 		                     *lspci*)
	;;            linux-nonfree)
	;;           (#t linux-libre)))
  ;;  (firmware (append (list linux-firmware-iwlwifi)
	;; 	                 %base-firmware))

	
  ;; Add GNOME and/or Xfce---we can choose at the log-in
  ;; screen with F1.  Use the "desktop" services, which
  ;; include the X11 log-in service, networking with
  ;; NetworkManager, and more.
  (services (cons* %desktop-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))

