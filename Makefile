TAR = tar
TAR_ARCHIVE = sublime.tar
TAR_OPTIONS = --transform 's|^|sublime-$(VERSION)/|'

all:
	$(TAR) cf $(TAR_ARCHIVE) $(TAR_OPTIONS) sublime.el sublime-pkg.el
