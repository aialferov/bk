
PROJECT := bk

LIBDIR := /usr/local/lib
BINDIR := /usr/local/bin
ETCDIR := /etc

COMPLETIONDIR := /etc/bash_completion.d
PROFILEDIR := /etc/profile.d

all:
	mkdir -p ebin
	erlc -o ebin/ src/*.erl

clean:
	rm -f ebin/*.beam
	rmdir --ignore-fail-on-non-empty -p ebin

install:
	mkdir -p $(LIBDIR)/$(PROJECT)/ebin
	install -p ebin/*.beam $(LIBDIR)/$(PROJECT)/ebin
	install -p ebin/bk.app $(LIBDIR)/$(PROJECT)/ebin
	install -p priv/bk $(BINDIR)
	install -p priv/bk.escript $(BINDIR)
	install -p -m 644 bk.conf $(ETCDIR)
	install -p -m 644 priv/bk_env.sh $(PROFILEDIR)
	install -p -m 644 priv/bk_bash_completion $(COMPLETIONDIR)/bk

uninstall:
	rm -f $(LIBDIR)/$(PROJECT)/ebin/bk.app
	rm -f $(LIBDIR)/$(PROJECT)/ebin/*.beam
	rm -f $(BINDIR)/bk
	rm -f $(BINDIR)/bk.escript
	rm -f $(ETCDIR)/bk.conf
	rm -f $(PROFILEDIR)/bk_env.sh
	rm -f $(COMPLETIONDIR)/bk
	rmdir --ignore-fail-on-non-empty -p $(LIBDIR)/$(PROJECT)/ebin
