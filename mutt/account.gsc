source "gpg -dq /home/klingenberg/.mutt/password.gsc.gpg |"
set ssl_starttls=yes
set ssl_force_tls=yes
set imap_user = 'klingenberg'
set from='klingenberg@gsc.tu-darmstadt.de'
set imap_pass=$my_pass
set realname='Dario Klingenberg'
set folder = imaps://mail.gsc.ce.tu-darmstadt.de/
set spoolfile = imaps://mail.gsc.ce.tu-darmstadt.de/INBOX
set postponed="imaps://mail.gsc.ce.tu-darmstadt.de/[gsc]/Drafts"
set header_cache = "~/.mutt/cache/headers"
set message_cachedir = "~/.mutt/cache/bodies"
set certificate_file = "~/.mutt/certificates"
set smtp_url = 'smtp://klingenberg@smtp.gsc.ce.tu-darmstadt.de:465/'
set move = no
set imap_keepalive = 900
set mailcap_path 	= ~/.mutt/mailcap
auto_view text/html
