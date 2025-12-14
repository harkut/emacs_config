;;; doxymacs-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "doxymacs" "doxymacs.el" (0 0 0 0))
;;; Generated autoloads from doxymacs.el

(autoload 'doxymacs-mode "doxymacs" "\
Minor mode for using/creating Doxygen documentation.
To submit a problem report, request a feature or get support, please
visit doxymacs' homepage at https://pniedzielski.github.io/doxymacs/.

This is a minor mode.  If called interactively, toggle the
`doxymacs mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `doxymacs-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

To see what version of doxymacs you are running, enter
`\\[doxymacs-version]'.

In order for `doxymacs-lookup' to work you will need to customise the
variable `doxymacs-doxygen-dirs'.

\(fn &optional ARG)" t nil)

(autoload 'doxymacs-install-external-parser "doxymacs" "\
Build and install the Doxygen external parser.

If `doxymacs-external-xml-parser-executable' does not exist, attempt to
rebuild it from source.

The external parser depends on the following packages:

  - autotools: https://www.gnu.org/software/autoconf/
  - pkg-config: https://www.freedesktop.org/wiki/Software/pkg-config/
  - libxml2: http://www.libxml.org/

Be sure these are properly configured and installed on your system
before running this function.

Once this function successfully compiles the external parser, you should
customize `doxymacs-use-external-xml-parser' to enable it." t nil)

(register-definition-prefixes "doxymacs" '("doxymacs-"))

;;;***

;;;### (autoloads nil "doxymacs-xml-parse" "doxymacs-xml-parse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doxymacs-xml-parse.el

(autoload 'doxymacs-xml-parse-read-xml "doxymacs-xml-parse" "\
Parse XML data at point into a Lisp structure.
See `doxymacs-xml-parse-insert-xml' for a description of the format of
this structure.  If PROGRESS-CALLBACK is specified, it will be invoked
occasionally with the percentage of the parsing that has been completed.
Point is left at the end of the XML structure read.

\(fn &optional PROGRESS-CALLBACK)" nil nil)

(autoload 'doxymacs-xml-parse-insert-xml "doxymacs-xml-parse" "\
Insert DATA, a recursive Lisp structure, at point as XML.
DATA has the form:

ENTRY       ::=  (TAG CHILD*)
CHILD       ::=  STRING | ENTRY
TAG         ::=  TAG_NAME | (TAG_NAME ATTR+)
ATTR        ::=  (ATTR_NAME . ATTR_VALUE)
TAG_NAME    ::=  STRING
ATTR_NAME   ::=  STRING
ATTR_VALUE  ::=  STRING

If ADD-NEWLINES is non-nil, newlines and indentation will be added to
make the data user-friendly.

If PUBLIC and SYSTEM are non-nil, a !DOCTYPE tag will be added at the
top of the document to identify it as an XML document.

DEPTH is normally for internal use only, and controls the depth of the
indentation.

\(fn DATA &optional ADD-NEWLINES PUBLIC SYSTEM DEPTH)" nil nil)

(autoload 'doxymacs-xml-parse-xml-reformat-tags "doxymacs-xml-parse" "\
If point is on the open bracket of an XML tag, reformat that tree.
Note that this only works if the opening tag starts at column 0." t nil)

(register-definition-prefixes "doxymacs-xml-parse" '("doxymacs-xml-parse--xml-"))

;;;***

;;;### (autoloads nil nil ("doxymacs-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; doxymacs-autoloads.el ends here
