#!/bin/sh
LISP=sbcl
LISP_OPTIONS="--noinform --load"

LOG4CL_URL="http://common-lisp.net/cgi-bin/viewcvs.cgi/log4cl.tar.gz?root=log4cl&view=tar"
LOG4CL_TGZ=log4cl.tar.gz

# download log4cl
wget $LOG4CL_URL -O $LOG4CL_TGZ
if [ "$?" != "0" ] ; then
    echo cannot DL log4cl tar ball
    exit 1
fi



# start lisp
$LISP $LISP_OPTIONS install-packages.lisp

