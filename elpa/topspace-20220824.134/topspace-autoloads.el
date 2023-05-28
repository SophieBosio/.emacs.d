;;; topspace-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "topspace" "topspace.el" (0 0 0 0))
;;; Generated autoloads from topspace.el

(autoload 'topspace-height "topspace" "\
Return the top space height in lines for current buffer in selected window.
The top space is the empty region in the buffer above the top text line.
The return value is a floating-point number, and is equivalent to
the top space pixel height / `default-line-height'.

If the height does not exist yet, zero will be returned if
`topspace-autocenter-buffers' returns nil, otherwise a value that centers
the buffer will be returned according to `topspace-center-position'.

If the stored height is now invalid, it will first be corrected by
`topspace--correct-height' before being returned.
Valid top space line heights are:
- never negative,
- only positive when `window-start' equals 1,
  `topspace-active' returns non-nil, and `topspace-mode' is enabled,
- not larger than `window-text-height' minus `topspace--context-lines'." nil nil)

(autoload 'topspace-set-height "topspace" "\
Set and redraw the top space overlay to have a target height of TOTAL-LINES.
This sets the top space height for the current buffer in the selected window.
Integer or floating-point numbers are accepted for TOTAL-LINES, and the value is
considered to be in units of `default-line-height'.

If argument TOTAL-LINES is not provided, the top space height will be set to
the value returned by `topspace-height', which can be useful when redrawing a
previously stored top space height in a window after a new buffer is
displayed in it, or when first setting the height to an initial default value
according to `topspace-height'.

If TOTAL-LINES is invalid, it will be corrected by `topspace--correct-height'.
Valid top space line heights are:
- never negative,
- only positive when `window-start' equals 1,
  `topspace-active' returns non-nil, and `topspace-mode' is enabled,
- not larger than `window-text-height' minus `topspace--context-lines'.

\(fn &optional TOTAL-LINES)" t nil)

(autoload 'topspace-recenter-buffer "topspace" "\
Add enough top space to center small buffers according to POSITION.
POSITION defaults to `topspace-center-position'.
Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height, or if `window-start' is greater
than 1.

If POSITION is a floating-point, it represents the position to center buffer as
a ratio of frame height, and can be a value from 0.0 to 1.0 where lower values
center the buffer higher up in the screen.

If POSITION is a positive or negative integer value, buffer will be centered
by putting its center line at a distance of `topspace-center-position' lines
away from the top of the selected window when positive, or from the bottom
of the selected window when negative.
The distance will be in units of lines with height `default-line-height',
and the value should be less than the height of the window.

Top space will not be added if the number of text lines in the buffer is larger
than or close to the selected window's height, or if `window-start' is greater
than 1.

Customize `topspace-center-position' to adjust the default centering position.
Customize `topspace-autocenter-buffers' to run this command automatically
after first opening buffers and after window sizes change.

\(fn &optional POSITION)" t nil)

(autoload 'topspace-default-active "topspace" "\
Default function that `topspace-active' is set to.
Return nil if the selected window is in a child-frame." nil nil)

(autoload 'topspace-default-autocenter-buffers "topspace" "\
Default function that `topspace-autocenter-buffers' is set to.
Return nil if the selected window is in a child-frame or user has scrolled
buffer in selected window." nil nil)

(autoload 'topspace-default-empty-line-indicator "topspace" "\
Default function that `topspace-empty-line-indicator' is set to.
Put the empty-line bitmap in fringe if `indicate-empty-lines' is non-nil.
This is done by adding a 'display property to the returned string.
The bitmap used is the one that the `empty-line' logical fringe indicator
maps to in `fringe-indicator-alist'." nil nil)

(autoload 'topspace-buffer-was-scrolled-p "topspace" "\
Return t if current buffer has been scrolled in the selected window before.
This is provided since it is used in `topspace-default-autocenter-buffers'.
Scrolling is only recorded if topspace is active in the buffer at the time of
scrolling." nil nil)

(autoload 'topspace-mode "topspace" "\
Recenter line 1 with scrollable upper margin/padding.

TopSpace lets you display a buffer's first line in the center of a window
instead of just at the top.
This is done by automatically drawing an upper margin/padding above line 1
as you recenter and scroll it down.

See https://github.com/trevorpogue/topspace for a GIF demo & documentation.

Features:

- Easier on the eyes: Recenter or scroll down top text to a more
  comfortable eye level for reading, especially when in full-screen
  or on a large monitor.

- Easy to use: No new keybindings are required, keep using all
  your previous scrolling & recentering commands, except now you
  can also scroll above the top lines.  It also integrates
  seamlessly with `centered-cursor-mode' to keep the cursor
  centered all the way to the top line.

Enabling/disabling:
When called interactively, toggle `topspace-mode'.

With prefix ARG, enable `topspace-mode' if
ARG is positive, otherwise disable it.

When called from Lisp, enable `topspace-mode' if
ARG is omitted, nil or positive.

If ARG is `toggle', toggle `topspace-mode'.
Otherwise behave as if called interactively.

\(fn &optional ARG)" t nil)

(put 'global-topspace-mode 'globalized-minor-mode t)

(defvar global-topspace-mode nil "\
Non-nil if Global Topspace mode is enabled.
See the `global-topspace-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-topspace-mode'.")

(custom-autoload 'global-topspace-mode "topspace" nil)

(autoload 'global-topspace-mode "topspace" "\
Toggle Topspace mode in all buffers.
With prefix ARG, enable Global Topspace mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Topspace mode is enabled in all buffers where `topspace-mode' would do
it.

See `topspace-mode' for more information on Topspace mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "topspace" '("topspace-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; topspace-autoloads.el ends here
