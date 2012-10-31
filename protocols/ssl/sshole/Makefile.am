SUBDIRS =  
dist-hook:
	cd $(top_distdir); subs=`egrep "^[:space:]*SUBDIRS" Makefile.am | sed 's/SUBDIRS[ =]\+//g'`; \
	echo "SUBDIRS =  $$subs" >>.makefile.am; \
	egrep -v "^[:space:]*SUBDIRS" Makefile.am >>.makefile.am; \
	mv .makefile.am Makefile.am; WANT_AUTOCONF=2.1 WANT_AUTOMAKE=1.4 autoconf && automake

# END OF MOTOR DIST TARGETS #
bin_PROGRAMS = sshole
sshole_SOURCES = sshole.c
EXTRA_DIST = sshole.c sshole.motor NEWS README AUTHORS INSTALL ChangeLog COPYING sshole.spec 
AUTOMAKE_OPTIONS = foreign
