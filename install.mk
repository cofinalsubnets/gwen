# installlation
# default install to home directory under ~/.local/

DESTDIR ?= $(HOME)/
PREFIX ?= .local/
VIMPREFIX ?= .vim/
vimdir=$(DESTDIR)$(VIMPREFIX)
dest=$(DESTDIR)$(PREFIX)
# all installed file paths
installed_binary=$(DESTDIR)/$(PREFIX)/bin/$n
installed_manpage=$(DESTDIR)/$(PREFIX)/share/man/man1/$n.1
installed_shared_library=$(DESTDIR)/$(PREFIX)/lib/lib$n.so
installed_header_file=$(DESTDIR)/$(PREFIX)/include/$n.h
installed_vim_ftdetect=$(DESTDIR)/$(VIMPREFIX)/ftdetect/$n.vim
installed_vim_syntax=$(DESTDIR)/$(VIMPREFIX)/syntax/$n.vim
installs=\
	$(installed_binary)\
 	$(installed_vim_syntax)\
	$(installed_vim_ftdetect)\
 	$(installed_manpage)\
	$(installed_shared_library)\
	$(installed_header_file)

install: $(installs)
uninstall:
	rm -f $(installs)

$(installed_header_file): src/$n.h
	install -D -m 644 $< $@
$(installed_shared_library): host/lib$n.so
	install -D -m 755 -s $< $@
$(installed_binary): host/$n
	install -D -m 755 -s $< $@
$(installed_vim_ftdetect): vim/ftdetect/$n.vim
	install -D -m 644 $< $@
$(installed_vim_syntax): vim/syntax/$n.vim
	install -D -m 644 $< $@
$(installed_manpage): host/$n.1
	install -D -m 644 $< $@
