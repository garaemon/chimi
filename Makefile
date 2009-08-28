######################################
# chimi Makefile for sbcl package
#
# expected directory configuration
# package-dir/
#      Makefile
#      src/
#        package-name.asd
#        package-name.lisp
#        hoge.lisp
#        fuga.lisp
#      doc/
#        gen-doc.lisp
#        html/
#      samples/
#      tests/
#        test-top.lisp
#      INSTALL
#      README
######################################

PACKAGE_NAME       = chimi
VERSION            = 0.0.0
TARBALL            = $(PACKAGE_NAME)-$(VERSION).tar.gz
TARBALL_CONTENTS   = Makefile INSTALL README samples tests doc src
LISP               = sbcl
LISP_OPTIONS       = --noinform --load
LISP_SYSTEM_DIR    = $(HOME)/.sbcl/systems
LISP_SITE_DIR      = $(HOME)/.sbcl/site
SSH                = ssh
SCP                = scp
LN                 = ln -sf
RM                 = rm -f
PACKAGE_ASD_PATH   = $(LISP_SYSTEM_DIR)/$(PACKAGE_NAME).asd
PACKAGE_TARGET_DIR = $(LISP_SITE_DIR)/$(PACKAGE_NAME)
# directories
PACKAGE_SRC_DIR    = $(PWD)/src
DOC_DIR            = $(PWD)/doc
HTML_DIR           = $(DOC_DIR)/html
MANUAL_DIR         = $(HTML_DIR)/manual
BASE_URL           = garaemon.net
WWW_DIR            = www
GEN_DOC_LISP       = $(PWD)/doc/gen-doc.lisp
TEST_LISP          = $(PWD)/tests/test-top.lisp


all: install-src install-asd

install-src:
	$(LN) $(PACKAGE_SRC_DIR) $(PACKAGE_TARGET_DIR)

install-asd:
	$(LN) $(PACKAGE_SRC_DIR)/$(PACKAGE_NAME).asd $(PACKAGE_ASD_PATH)

clean:
	$(RM) $(PACKAGE_ASD_PATH) $(PACKAGE_TARGET_DIR)
	$(RM) src/*fasl

test:
	$(LISP) $(LISP_OPTIONS) $(TEST_LISP) 2>/dev/null

gen-doc:
	$(LISP) $(LISP_OPTIONS) $(GEN_DOC_LISP)

clean-doc:
	$(RM) -r $(MANUAL_DIR)

www: tarball
	ssh $(BASE_URL) "mkdir -p ~/$(WWW_DIR)/$(PACKAGE_NAME)/"
	ssh $(BASE_URL) "mkdir -p ~/$(WWW_DIR)/$(PACKAGE_NAME)/archives"
	$(SCP) -r $(HTML_DIR)/* $(BASE_URL):~/$(WWW_DIR)/$(PACKAGE_NAME)/
	$(SCP) $(TARBALL) $(BASE_URL):~/$(WWW_DIR)/$(PACKAGE_NAME)/archives/

tarball: clean clean-tarball
	mkdir $(PWD)/$(PACKAGE_NAME)
	cp -r $(TARBALL_CONTENTS) $(PWD)/$(PACKAGE_NAME)
	tar cvzf $(TARBALL) $(PWD)/$(PACKAGE_NAME)
	$(RM) -r $(PWD)/$(PACKAGE_NAME)

clean-tarball:
	$(RM) $(TARBALL)

dist-clean: clean clean-doc clean-tarball
