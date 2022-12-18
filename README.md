README
======

This is my emacs configuration, placed on github for portability and
general sharing.  It has accumulated a decent amount of cruft over the
years, so please feel free to point out where it can be improved.

These days I have a fairly homogenous environment across my different
installations, so there's not a lot of effort put in to making things
backwards-compatible. I mostly run a snapshot build on a Linux
(Ubuntu) host, and the [latest
build](https://github.com/kiennq/emacs-build) on windows. Most
configuration should be portable, but there are facilities to load
platform-specific (and host-specific) code when necessary. I assume a
very recent Emacs.

Installation
------------

This should be as simple as cloning from github.  If you check out to
`~/.emacs.d` then you are finished.  Otherwise, you can place it
where-ever you feel like and just symlink `init.el` to `~/.emacs`; it
is designed to be portable like this.

I use [use-package](https://github.com/jwiegley/use-package) now,
after years with [el-get](https://github.com/dimitri/el-get).  There
are a couple of helpers for packages that are only on github, or
sometimes at a URL (which I host on gists).

General Notes
-------------

The top-level file is init.el; it basically just adds its parent
directory (which should be where you checked this out) to the
load-path, and loads a bunch of mode- and task-specific files.  These
are by convention named `custom-<task>.el`, mainly so they will be
grouped together in a directory listing.  The exception is the
allowance for platform-specific initialisation, which is loaded by
looking for a file with the same name as the `symbol-value` of
`system-type`.

For efficiency, I have borrowed a few hints from
[Jacob Gabrielson's post on effective emacs](http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html),
in particular setting things up for autoload where possible, and as a
result making heavy use of `eval-after-load`.  This might occasionally
make it a bit harder to read.

There's a bit of defensive coding; in place of `(require 'foo)`
followed by configuration, I tend to use `(when (require 'foo nil t)
...)` instead.  The last argument to `require` means to simply return
`nil` but not signal an error in the event that a feature cannot be
loaded.  Yes, this all could and possibly should be wrapped as a
macro.

TODO:
-----

Look at
[https support for package.el](https://glyph.twistedmatrix.com/2015/11/editor-malware.html)
