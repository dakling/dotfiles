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
set smtp_url = 'smtps://klingenberg@smtp.gsc.ce.tu-darmstadt.de:465/'
set smtp_pass=$my_pass
set move = no
set imap_keepalive = 900
set mailcap_path 	= ~/.mutt/mailcap
set signature="~/.dotfiles/dotfiles/mutt/sig.gsc"
auto_view text/html
set timeout=30
set query_command= "abook --mutt-query '%s'"
macro index,pager  a "<pipe-message>abook --add-email-quiet<return>" "Add this sender to Abook"
bind editor        <Tab> complete-query
set pager=nvimpager
auto_view text/calendar
macro index ",c" "<pipe-message>~/.mutt/parse_remind.pl<enter>"
