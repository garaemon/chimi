######################################
# Makefile for chimi
######################################

LISP             = sbcl
LISP_OPTIONS     = --noinform --load
LISP_SYSTEM_DIR  = $(HOME)/.sbcl/systems
LISP_SITE_DIR    = $(HOME)/.sbcl/site
CHIMI_ASD_PATH   = $(LISP_SYSTEM_DIR)/chimi.asd
CHIMI_TARGET_DIR = $(LISP_SITE_DIR)/chimi
CHIMI_SRC_DIR    = $(PWD)/src

LN=ln -sf
RM=rm -f
LOG4CL_URL="http://common-lisp.net/cgi-bin/viewcvs.cgi/log4cl.tar.gz?root=log4cl&view=tar"
LOG4CL_TGZ=log4cl.tar.gz

all: install-chimi-src install-chimi-asd

install-chimi-src:
	$(LN) $(CHIMI_SRC_DIR) $(CHIMI_TARGET_DIR)

install-chimi-asd:
	$(LN) $(CHIMI_SRC_DIR)/chimi.asd $(CHIMI_ASD_PATH)

install-log4cl:
	wget $(LOG4CL_URL) -O /tmp/$(LOG4CL_TGZ)
	$(LISP) --eval (progn (require :asdf-install) (asdf-install "/tmp/$(LOG4CL_TGZ)"))

clean:
	$(RM) $(CHIMI_ASD_PATH) $(CHIMI_TARGET_DIR)
	$(RM) src/*fasl

test:
	$(LISP) $(LISP_OPTIONS) $(PWD)/tests/test-chimi.lisp 2>/dev/null
www:

