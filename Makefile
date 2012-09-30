VERSION = 0.1.0

TAR = tar
TAR_ARCHIVE = sublime-$(VERSION).tar
TAR_OPTIONS = --transform 's|^|sublime-$(VERSION)/|'

MANIFEST = sublime-pkg.el
MANIFEST_IN = $(MANIFEST).in

SOURCES = sublime.el   \
	sublime-keys.el	   \
	sublime-project.el \
	sublime-support.el \
	sublime-system.el  \
	sublime-ui.el


all: dist


sublime-pkg.el:
	sed 's|@@VERSION@@|$(VERSION)|' $(MANIFEST_IN) > $@


dist: sublime-pkg.el
	$(TAR) cf $(TAR_ARCHIVE) $(TAR_OPTIONS) $(SOURCES) $(MANIFEST)


clean:
	rm -f *.tar
	rm -f $(MANIFEST)


.PHONY : clean
