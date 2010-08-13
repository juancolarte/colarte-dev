#!/bin/sh
RUTA=/etc/init.d
for i in amavis apache clamav-daemon clamav-freshclam courier-imap courier-pop exim4 mysql spamassassin saslauthd postfix vmware xprint;
    do $RUTA/$i stop;
    done
    
    
