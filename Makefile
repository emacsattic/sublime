TAR = tar
TAR_ARCHIVE = sublime.tar
TAR_OPTIONS = --transform 's,^,/sublime-0.0.1/,'

all:
	$(TAR) cf $(TAR_ARCHIVE) $(TAR_OPTIONS) sublime.el sublime-pkg.el
