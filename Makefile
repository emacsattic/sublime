VERSION = 0.0.6

TAR = tar
TAR_ARCHIVE = sublime-$(VERSION).tar
TAR_OPTIONS = --transform 's|^|sublime-$(VERSION)/|'

prereq:
	@test -n "$(VERSION)" || ( echo "Forgot VERSION= ?"; exit 1 )

sublime-pkg.el: prereq
	sed 's|@@VERSION@@|$(VERSION)|' sublime-pkg.el.in > $@

all: prereq sublime-pkg.el
	$(TAR) cf $(TAR_ARCHIVE) $(TAR_OPTIONS) sublime.el sublime-pkg.el
	rm -f sublime-pkg.el

clean:
	rm -f *.tar
