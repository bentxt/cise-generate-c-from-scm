#
# $Id: template.Makefile.in,v 1.7 2007/08/10 06:16:44 shirok Exp $
#

# General info
SHELL       = /bin/sh
prefix      = /usr/local
exec_prefix = ${prefix}
bindir      = ${exec_prefix}/bin
libdir      = ${exec_prefix}/lib
datadir     = ${datarootdir}
datarootdir = ${prefix}/share
srcdir      = .


# These may be overridden by make invocators
DESTDIR        =
GOSH           = /usr/local/bin/gosh
GAUCHE_CONFIG  = /usr/local/bin/gauche-config
GAUCHE_PACKAGE = /usr/local/bin/gauche-package
INSTALL        = /usr/local/bin/gauche-install

# Other parameters
SOEXT  = so
OBJEXT = o
EXEEXT = 

# Module-specific stuff
PACKAGE   = dyncomp

ARCHFILES = dyncomp.$(SOEXT)
SCMFILES  = $(srcdir)/dyncomp.scm
HEADERS   = 
TCC_HEADERS = stdarg.h stddef.h stdbool.h float.h varargs.h tcclib.h
DYNCOMP_HELPER_H = dyncomp_helper_darwin.h

TARGET    = $(ARCHFILES) dyncomp_helper.h
GENERATED = $(srcdir)/sys_inc_paths.h
CONFIG_GENERATED = Makefile config.cache config.log config.status \
		   configure.lineno autom4te*.cache $(PACKAGE).gpd

GAUCHE_PKGINCDIR  = $(DESTDIR)${libdir}/gauche/site/include
GAUCHE_PKGLIBDIR  = $(DESTDIR)${datadir}/gauche/site/lib
GAUCHE_PKGARCHDIR = $(DESTDIR)${libdir}/gauche/site/0.8.14/i386-apple-darwin9.5.0

DYNCOMP_DIR       = $(GAUCHE_PKGLIBDIR)/dyncomp
TCC_INCLUDE_DIR   = $(DYNCOMP_DIR)/include

dyncomp_SRCS = $(srcdir)/dyncomp.c $(srcdir)/dyncomplib.stub

all : $(TARGET)

tcc-0.9.24/config.mak:
	cd tcc-0.9.24; ./configure --includedir=$(TCC_INCLUDE_DIR)

tcc-0.9.24/libtcc.a: tcc-0.9.24/config.mak
	cd tcc-0.9.24; $(MAKE) libtcc.a

tcc-0.9.24/libtcc1.a: tcc-0.9.24/config.mak
	cd tcc-0.9.24; $(MAKE) libtcc1.a

$(srcdir)/sys_inc_paths.h:
	$(GOSH) genheader.scm $(TCC_INCLUDE_DIR) > $(srcdir)/sys_inc_paths.h

dyncomp.$(SOEXT): $(dyncomp_SRCS) tcc-0.9.24/libtcc.a tcc-0.9.24/libtcc1.a $(srcdir)/sys_inc_paths.h
	$(GAUCHE_PACKAGE) compile --cppflags="-I." --libs="-L./tcc-0.9.24 -ltcc -ltcc1" --verbose dyncomp $(dyncomp_SRCS)

dyncomp_helper.h: $(DYNCOMP_HELPER_H)
	cp $(DYNCOMP_HELPER_H) dyncomp_helper.h

check : all
	@rm -f test.log
	$(GOSH) -I. -I$(srcdir) $(srcdir)/test.scm > test.log

install : all
	$(INSTALL) -m 444 -T $(GAUCHE_PKGINCDIR) $(HEADERS)
	$(INSTALL) -m 444 -T $(GAUCHE_PKGLIBDIR) $(SCMFILES) 
	$(INSTALL) -m 555 -T $(GAUCHE_PKGARCHDIR) $(ARCHFILES)
	$(INSTALL) -m 444 -T $(GAUCHE_PKGLIBDIR)/.packages $(PACKAGE).gpd
	mkdir -p $(TCC_INCLUDE_DIR)
	$(INSTALL) -m 444 -T $(TCC_INCLUDE_DIR) dyncomp_helper.h
	cd tcc-0.9.24; $(INSTALL) -m 444 -T $(TCC_INCLUDE_DIR) $(TCC_HEADERS)

uninstall :
	$(INSTALL) -U $(GAUCHE_PKGINCDIR) $(HEADERS)
	$(INSTALL) -U $(GAUCHE_PKGLIBDIR) $(SCMFILES) 
	$(INSTALL) -U $(GAUCHE_PKGARCHDIR) $(ARCHFILES)
	$(INSTALL) -U $(GAUCHE_PKGLIBDIR)/.packages $(PACKAGE).gpd
	rm -Rf $(TCC_INCLUDE_DIR) $(DYNCOMP_DIR)

clean :
	$(GAUCHE_PACKAGE) compile --clean dyncomp $(dyncomp_SRCS)
	cd tcc-0.9.24; $(MAKE) clean
	rm -rf core $(TARGET) $(GENERATED) *~ test.log so_locations

distclean : clean
	cd tcc-0.9.24; $(MAKE) distclean
	rm -rf $(CONFIG_GENERATED)

maintainer-clean : clean
	cd tcc-0.9.24; $(MAKE) distclean
	rm -rf $(CONFIG_GENERATED) configure VERSION DIST_EXCLUDE_X

