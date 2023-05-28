;;; treemacs-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "treemacs" "treemacs.el" (0 0 0 0))
;;; Generated autoloads from treemacs.el

(autoload 'treemacs-version "treemacs" "\
Return the `treemacs-version'." t nil)

(autoload 'treemacs "treemacs" "\
Initialise or toggle treemacs.
- If the treemacs window is visible hide it.
- If a treemacs buffer exists, but is not visible show it.
- If no treemacs buffer exists for the current frame create and show it.
- If the workspace is empty additionally ask for the root path of the first
  project to add.
- With a prefix ARG launch treemacs and force it to select a workspace

\(fn &optional ARG)" t nil)

(autoload 'treemacs-select-directory "treemacs" "\
Select a directory to open in treemacs.
This command will open *just* the selected directory in treemacs.  If there are
other projects in the workspace they will be removed.

To *add* a project to the current workspace use
`treemacs-add-project-to-workspace' or
`treemacs-add-and-display-current-project' instead." t nil)

(autoload 'treemacs-find-file "treemacs" "\
Find and focus the current file in the treemacs window.
If the current buffer has visits no file or with a prefix ARG ask for the
file instead.
Will show/create a treemacs buffers if it is not visible/does not exist.
For the most part only useful when `treemacs-follow-mode' is not active.

\(fn &optional ARG)" t nil)

(autoload 'treemacs-find-tag "treemacs" "\
Find and move point to the tag at point in the treemacs view.
Most likely to be useful when `treemacs-tag-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root.  If no treemacs buffer exists it will be created with the current file's
containing directory as root.  Will do nothing if the current buffer is not
visiting a file or Emacs cannot find any tags for the current file." t nil)

(autoload 'treemacs-select-window "treemacs" "\
Select the treemacs window if it is visible.
Bring it to the foreground if it is not visible.
Initialise a new treemacs buffer as calling `treemacs' would if there is no
treemacs buffer for this frame.

In case treemacs is already selected behaviour will depend on
`treemacs-select-when-already-in-treemacs'.

A non-nil prefix ARG will also force a workspace switch.

\(fn &optional ARG)" t nil)

(autoload 'treemacs-show-changelog "treemacs" "\
Show the changelog of treemacs." t nil)

(autoload 'treemacs-edit-workspaces "treemacs" "\
Edit your treemacs workspaces and projects as an `org-mode' file." t nil)

(autoload 'treemacs-add-and-display-current-project-exclusively "treemacs" "\
Display the current project, and *only* the current project.
Like `treemacs-add-and-display-current-project' this will add the current
project to treemacs based on either projectile, the built-in project.el, or the
current working directory.

However the \\='exclusive\\=' part means that it will make the current project
the only project, all other projects *will be removed* from the current
workspace." t nil)

(autoload 'treemacs-add-and-display-current-project "treemacs" "\
Open treemacs and add the current project root to the workspace.
The project is determined first by projectile (if treemacs-projectile is
installed), then by project.el, then by the current working directory.

If the project is already registered with treemacs just move point to its root.
An error message is displayed if the current buffer is not part of any project." t nil)

(register-definition-prefixes "treemacs" '("treemacs-version"))

;;;***

;;;### (autoloads nil "treemacs-annotations" "treemacs-annotations.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-annotations.el

(register-definition-prefixes "treemacs-annotations" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-async" "treemacs-async.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from treemacs-async.el

(register-definition-prefixes "treemacs-async" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-bookmarks" "treemacs-bookmarks.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-bookmarks.el

(autoload 'treemacs-bookmark "treemacs-bookmarks" "\
Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location, in a similar
fashion to `treemacs-find-file'.

With a prefix argument ARG treemacs will also open the bookmarked location.

\(fn &optional ARG)" t nil)

(autoload 'treemacs--bookmark-handler "treemacs-bookmarks" "\
Open Treemacs into a bookmark RECORD.

\(fn RECORD)" nil nil)

(autoload 'treemacs-add-bookmark "treemacs-bookmarks" "\
Add the current node to Emacs' list of bookmarks.
For file and directory nodes their absolute path is saved.  Tag nodes
additionally also save the tag's position.  A tag can only be bookmarked if the
treemacs node is pointing to a valid buffer position." t nil)

(register-definition-prefixes "treemacs-bookmarks" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-compatibility" "treemacs-compatibility.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-compatibility.el

(register-definition-prefixes "treemacs-compatibility" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-core-utils" "treemacs-core-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-core-utils.el

(register-definition-prefixes "treemacs-core-utils" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-customization" "treemacs-customization.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-customization.el

(register-definition-prefixes "treemacs-customization" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-diagnostics" "treemacs-diagnostics.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-diagnostics.el

(register-definition-prefixes "treemacs-diagnostics" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-dom" "treemacs-dom.el" (0 0 0 0))
;;; Generated autoloads from treemacs-dom.el

(register-definition-prefixes "treemacs-dom" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-extensions" "treemacs-extensions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-extensions.el

(register-definition-prefixes "treemacs-extensions" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-file-management" "treemacs-file-management.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-file-management.el

(autoload 'treemacs-delete-file "treemacs-file-management" "\
Delete node at point.
A delete action must always be confirmed.  Directories are deleted recursively.
By default files are deleted by moving them to the trash.  With a prefix ARG
they will instead be wiped irreversibly.

\(fn &optional ARG)" t nil)

(autoload 'treemacs-delete-marked-files "treemacs-file-management" "\
Delete all marked files.

A delete action must always be confirmed.  Directories are deleted recursively.
By default files are deleted by moving them to the trash.  With a prefix ARG
they will instead be wiped irreversibly.

For marking files see `treemacs-bulk-file-actions'.

\(fn &optional ARG)" t nil)

(autoload 'treemacs-move-file "treemacs-file-management" "\
Move file (or directory) at point.

If the selected target is an existing directory the source file will be directly
moved into this directory.  If the given target instead does not exist then it
will be treated as the moved file's new name, meaning the original source file
will be both moved and renamed." t nil)

(autoload 'treemacs-copy-file "treemacs-file-management" "\
Copy file (or directory) at point.

If the selected target is an existing directory the source file will be directly
copied into this directory.  If the given target instead does not exist then it
will be treated as the copied file's new name, meaning the original source file
will be both copied and renamed." t nil)

(autoload 'treemacs-move-marked-files "treemacs-file-management" "\
Move all marked files.

For marking files see `treemacs-bulk-file-actions'." t nil)

(autoload 'treemacs-copy-marked-files "treemacs-file-management" "\
Copy all marked files.

For marking files see `treemacs-bulk-file-actions'." t nil)

(autoload 'treemacs-rename-file "treemacs-file-management" "\
Rename the file/directory at point.

Buffers visiting the renamed file or visiting a file inside the renamed
directory and windows showing them will be reloaded.  The list of recent files
will likewise be updated." t nil)

(autoload 'treemacs-show-marked-files "treemacs-file-management" "\
Print a list of all files marked by treemacs." t nil)

(autoload 'treemacs-mark-or-unmark-path-at-point "treemacs-file-management" "\
Mark or unmark the absolute path of the node at point." t nil)

(autoload 'treemacs-reset-marks "treemacs-file-management" "\
Unmark all previously marked files in the current buffer." t nil)

(autoload 'treemacs-delete-marked-paths "treemacs-file-management" "\
Delete all previously marked files." t nil)

(autoload 'treemacs-bulk-file-actions "treemacs-file-management" "\
Activate the bulk file actions hydra.
This interface allows to quickly (unmark) files, so as to copy, move or delete
them in bulk.

Note that marking files is *permanent*, files will stay marked until they are
either manually unmarked or deleted.  You can show a list of all currently
marked files with `treemacs-show-marked-files' or `s' in the hydra." t nil)

(autoload 'treemacs-create-file "treemacs-file-management" "\
Create a new file.
Enter first the directory to create the new file in, then the new file's name.
The pre-selection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using $HOME when there is no path at or near point to grab." t nil)

(autoload 'treemacs-create-dir "treemacs-file-management" "\
Create a new directory.
Enter first the directory to create the new dir in, then the new dir's name.
The pre-selection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using $HOME when there is no path at or near point to grab." t nil)

(register-definition-prefixes "treemacs-file-management" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-filewatch-mode" "treemacs-filewatch-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-filewatch-mode.el

(register-definition-prefixes "treemacs-filewatch-mode" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-follow-mode" "treemacs-follow-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-follow-mode.el

(register-definition-prefixes "treemacs-follow-mode" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-fringe-indicator" "treemacs-fringe-indicator.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-fringe-indicator.el

(register-definition-prefixes "treemacs-fringe-indicator" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-git-commit-diff-mode" "treemacs-git-commit-diff-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-git-commit-diff-mode.el

(defvar treemacs-git-commit-diff-mode nil "\
Non-nil if Treemacs-Git-Commit-Diff mode is enabled.
See the `treemacs-git-commit-diff-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-git-commit-diff-mode'.")

(custom-autoload 'treemacs-git-commit-diff-mode "treemacs-git-commit-diff-mode" nil)

(autoload 'treemacs-git-commit-diff-mode "treemacs-git-commit-diff-mode" "\
Minor mode to display commit differences for your git-tracked projects.

This is a minor mode.  If called interactively, toggle the
`Treemacs-Git-Commit-Diff mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='treemacs-git-commit-diff-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When enabled treemacs will add an annotation next to every git project showing
how many commits ahead or behind your current branch is compared to its remote
counterpart.

The difference will be shown using the format `↑x ↓y', where `x' and `y' are the
numbers of commits a project is ahead or behind.  The numbers are determined
based on the output of `git status -sb'.

By default the annotation is only updated when manually updating a project with
`treemacs-refresh'.  You can install `treemacs-magit' to enable automatic
updates whenever you commit/fetch/rebase etc. in magit.

Does not require `treemacs-git-mode' to be active.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "treemacs-git-commit-diff-mode" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-header-line" "treemacs-header-line.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-header-line.el

(defvar treemacs-indicate-top-scroll-mode nil "\
Non-nil if Treemacs-Indicate-Top-Scroll mode is enabled.
See the `treemacs-indicate-top-scroll-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-indicate-top-scroll-mode'.")

(custom-autoload 'treemacs-indicate-top-scroll-mode "treemacs-header-line" nil)

(autoload 'treemacs-indicate-top-scroll-mode "treemacs-header-line" "\
Minor mode which shows whether treemacs is scrolled all the way to the top.

This is a minor mode.  If called interactively, toggle the
`Treemacs-Indicate-Top-Scroll mode' mode.  If the prefix argument
is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='treemacs-indicate-top-scroll-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When this mode is enabled the header line of the treemacs window will display
whether the window's first line is visible or not.

The strings used for the display are determined by
`treemacs-header-scroll-indicators'.

This mode makes use of `treemacs-user-header-line-format' - and thus
`header-line-format' - and is therefore incompatible with other modifications to
these options.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "treemacs-header-line" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-hydras" "treemacs-hydras.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from treemacs-hydras.el

(autoload 'treemacs-common-helpful-hydra "treemacs-hydras" "\
Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the most commonly used keybinds for treemacs.  For the more
advanced (probably rarely used keybinds) see `treemacs-advanced-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead." t nil)

(autoload 'treemacs-advanced-helpful-hydra "treemacs-hydras" "\
Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the more advanced (rarely used) keybinds for treemacs.  For
the more commonly used keybinds see `treemacs-common-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead." t nil)

(register-definition-prefixes "treemacs-hydras" '("treemacs-helpful-hydra"))

;;;***

;;;### (autoloads nil "treemacs-icons" "treemacs-icons.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from treemacs-icons.el

(autoload 'treemacs-resize-icons "treemacs-icons" "\
Resize the current theme's icons to the given SIZE.

If SIZE is \\='nil' the icons are not resized and will retain their default size
of 22 pixels.

There is only one size, the icons are square and the aspect ratio will be
preserved when resizing them therefore width and height are the same.

Resizing the icons only works if Emacs was built with ImageMagick support, or if
using Emacs >= 27.1,which has native image resizing support.  If this is not the
case this function will not have any effect.

Custom icons are not taken into account, only the size of treemacs' own icons
png are changed.

\(fn SIZE)" t nil)

(autoload 'treemacs-define-custom-icon "treemacs-icons" "\
Define a custom ICON for the current theme to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period.  This makes it possible to match file names like
\\='.gitignore' and \\='Makefile'.

Additionally FILE-EXTENSIONS are also not case sensitive and will be stored in a
down-cased state.

\(fn ICON &rest FILE-EXTENSIONS)" nil nil)

(autoload 'treemacs-define-custom-image-icon "treemacs-icons" "\
Same as `treemacs-define-custom-icon' but for image icons instead of strings.
FILE is the path to an icon image (and not the actual icon string).
FILE-EXTENSIONS are all the (not case-sensitive) file extensions the icon
should be used for.

\(fn FILE &rest FILE-EXTENSIONS)" nil nil)

(autoload 'treemacs-map-icons-with-auto-mode-alist "treemacs-icons" "\
Remaps icons for EXTENSIONS according to `auto-mode-alist'.
EXTENSIONS should be a list of file extensions such that they match the regex
stored in `auto-mode-alist', for example \\='(\".cc\").
MODE-ICON-ALIST is an alist that maps which mode from `auto-mode-alist' should
be assigned which treemacs icon, for example
`((c-mode . ,(treemacs-get-icon-value \"c\"))
  (c++-mode . ,(treemacs-get-icon-value \"cpp\")))

\(fn EXTENSIONS MODE-ICON-ALIST)" nil nil)

(register-definition-prefixes "treemacs-icons" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-interface" "treemacs-interface.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-interface.el

(register-definition-prefixes "treemacs-interface" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-logging" "treemacs-logging.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from treemacs-logging.el

(register-definition-prefixes "treemacs-logging" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-macros" "treemacs-macros.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from treemacs-macros.el

(register-definition-prefixes "treemacs-macros" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-mode" "treemacs-mode.el" (0 0 0 0))
;;; Generated autoloads from treemacs-mode.el

(autoload 'treemacs-mode "treemacs-mode" "\
A major mode for displaying the file system in a tree layout.

\(fn)" t nil)

(register-definition-prefixes "treemacs-mode" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-mouse-interface" "treemacs-mouse-interface.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-mouse-interface.el

(autoload 'treemacs-leftclick-action "treemacs-mouse-interface" "\
Move focus to the clicked line.
Must be bound to a mouse click, or EVENT will not be supplied.

\(fn EVENT)" t nil)

(autoload 'treemacs-doubleclick-action "treemacs-mouse-interface" "\
Run the appropriate double-click action for the current node.
In the default configuration this means to expand/collapse directories and open
files and tags in the most recently used window.

This function's exact configuration is stored in
`treemacs-doubleclick-actions-config'.

Must be bound to a mouse double click to properly handle a click EVENT.

\(fn EVENT)" t nil)

(autoload 'treemacs-single-click-expand-action "treemacs-mouse-interface" "\
A modified single-leftclick action that expands the clicked nodes.
Can be bound to <mouse1> if you prefer to expand nodes with a single click
instead of a double click.  Either way it must be bound to a mouse click, or
EVENT will not be supplied.

Clicking on icons will expand a file's tags, just like
`treemacs-leftclick-action'.

\(fn EVENT)" t nil)

(autoload 'treemacs-dragleftclick-action "treemacs-mouse-interface" "\
Drag a file/dir node to be opened in a window.
Must be bound to a mouse click, or EVENT will not be supplied.

\(fn EVENT)" t nil)

(autoload 'treemacs-define-doubleclick-action "treemacs-mouse-interface" "\
Define the behaviour of `treemacs-doubleclick-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.

The list of possible states can be found in `treemacs-valid-button-states'.
ACTION should be one of the `treemacs-visit-node-*' commands.

\(fn STATE ACTION)" nil nil)

(autoload 'treemacs-node-buffer-and-position "treemacs-mouse-interface" "\
Return source buffer or list of buffer and position for the current node.
This information can be used for future display.  Stay in the selected window
and ignore any prefix argument.

\(fn &optional _)" t nil)

(autoload 'treemacs-rightclick-menu "treemacs-mouse-interface" "\
Show a contextual right click menu based on click EVENT.

\(fn EVENT)" t nil)

(register-definition-prefixes "treemacs-mouse-interface" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-peek-mode" "treemacs-peek-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-peek-mode.el

(defvar treemacs-peek-mode nil "\
Non-nil if Treemacs-Peek mode is enabled.
See the `treemacs-peek-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-peek-mode'.")

(custom-autoload 'treemacs-peek-mode "treemacs-peek-mode" nil)

(autoload 'treemacs-peek-mode "treemacs-peek-mode" "\
Minor mode that allows you to peek at buffers before deciding to open them.

This is a minor mode.  If called interactively, toggle the
`Treemacs-Peek mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='treemacs-peek-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

While the mode is active treemacs will automatically display the file at point,
without leaving the treemacs window.

Peeking will stop when you leave the treemacs window, be it through a command
like `treemacs-RET-action' or some other window selection change.

Files' buffers that have been opened for peeking will be cleaned up if they did
not exist before peeking started.

The peeked window can be scrolled using
`treemacs-next/previous-line-other-window' and
`treemacs-next/previous-page-other-window'

\(fn &optional ARG)" t nil)

(register-definition-prefixes "treemacs-peek-mode" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-persistence" "treemacs-persistence.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-persistence.el

(register-definition-prefixes "treemacs-persistence" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-project-follow-mode" "treemacs-project-follow-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-project-follow-mode.el

(defvar treemacs-project-follow-mode nil "\
Non-nil if Treemacs-Project-Follow mode is enabled.
See the `treemacs-project-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-project-follow-mode'.")

(custom-autoload 'treemacs-project-follow-mode "treemacs-project-follow-mode" nil)

(autoload 'treemacs-project-follow-mode "treemacs-project-follow-mode" "\
Toggle `treemacs-only-current-project-mode'.

This is a minor mode.  If called interactively, toggle the
`Treemacs-Project-Follow mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='treemacs-project-follow-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This is a minor mode meant for those who do not care about treemacs' workspace
features, or its preference to work with multiple projects simultaneously.  When
enabled it will function as an automated version of
`treemacs-display-current-project-exclusively', making sure that, after a small
idle delay, the current project, and *only* the current project, is displayed in
treemacs.

The project detection is based on the current buffer, and will try to determine
the project using the following methods, in the order they are listed:

- the current projectile.el project, if `treemacs-projectile' is installed
- the current project.el project
- the current `default-directory'

The update will only happen when treemacs is in the foreground, meaning a
treemacs window must exist in the current scope.

This mode requires at least Emacs version 27 since it relies on
`window-buffer-change-functions' and `window-selection-change-functions'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "treemacs-project-follow-mode" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-rendering" "treemacs-rendering.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-rendering.el

(register-definition-prefixes "treemacs-rendering" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-scope" "treemacs-scope.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from treemacs-scope.el

(register-definition-prefixes "treemacs-scope" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-tag-follow-mode" "treemacs-tag-follow-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-tag-follow-mode.el

(autoload 'treemacs--flatten&sort-imenu-index "treemacs-tag-follow-mode" "\
Flatten current file's imenu index and sort it by tag position.
The tags are sorted into the order in which they appear, regardless of section
or nesting depth." nil nil)

(defvar treemacs-tag-follow-mode nil "\
Non-nil if Treemacs-Tag-Follow mode is enabled.
See the `treemacs-tag-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-tag-follow-mode'.")

(custom-autoload 'treemacs-tag-follow-mode "treemacs-tag-follow-mode" nil)

(autoload 'treemacs-tag-follow-mode "treemacs-tag-follow-mode" "\
Toggle `treemacs-tag-follow-mode'.

This is a minor mode.  If called interactively, toggle the
`Treemacs-Tag-Follow mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='treemacs-tag-follow-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This acts as more fine-grained alternative to `treemacs-follow-mode' and will
thus disable `treemacs-follow-mode' on activation.  When enabled treemacs will
focus not only the file of the current buffer, but also the tag at point.

The follow action is attached to Emacs' idle timer and will run
`treemacs-tag-follow-delay' seconds of idle time.  The delay value is not an
integer, meaning it accepts floating point values like 1.5.

Every time a tag is followed a re--scan of the imenu index is forced by
temporarily setting `imenu-auto-rescan' to t (though a cache is applied as long
as the buffer is unmodified).  This is necessary to assure that creation or
deletion of tags does not lead to errors and guarantees an always up-to-date tag
view.

Note that in order to move to a tag in treemacs the treemacs buffer's window
needs to be temporarily selected, which will reset blink-cursor-mode's timer if
it is enabled.  This will result in the cursor blinking seemingly pausing for a
short time and giving the appearance of the tag follow action lasting much
longer than it really does.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "treemacs-tag-follow-mode" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-tags" "treemacs-tags.el" (0 0 0 0))
;;; Generated autoloads from treemacs-tags.el

(autoload 'treemacs--expand-file-node "treemacs-tags" "\
Open tag items for file BTN.
Recursively open all tags below BTN when RECURSIVE is non-nil.

\(fn BTN &optional RECURSIVE)" nil nil)

(autoload 'treemacs--collapse-file-node "treemacs-tags" "\
Close node given by BTN.
Remove all open tag entries under BTN when RECURSIVE.

\(fn BTN &optional RECURSIVE)" nil nil)

(autoload 'treemacs--visit-or-expand/collapse-tag-node "treemacs-tags" "\
Visit tag section BTN if possible, expand or collapse it otherwise.
Pass prefix ARG on to either visit or toggle action.

FIND-WINDOW is a special provision depending on this function's invocation
context and decides whether to find the window to display in (if the tag is
visited instead of the node being expanded).

On the one hand it can be called based on `treemacs-RET-actions-config' (or
TAB).  The functions in these configs are expected to find the windows they need
to display in themselves, so FIND-WINDOW must be t. On the other hand this
function is also called from the top level vist-node functions like
`treemacs-visit-node-vertical-split' which delegates to the
`treemacs--execute-button-action' macro which includes the determination of
the display window.

\(fn BTN ARG FIND-WINDOW)" nil nil)

(autoload 'treemacs--expand-tag-node "treemacs-tags" "\
Open tags node items for BTN.
Open all tag section under BTN when call is RECURSIVE.

\(fn BTN &optional RECURSIVE)" nil nil)

(autoload 'treemacs--collapse-tag-node "treemacs-tags" "\
Close tags node at BTN.
Remove all open tag entries under BTN when RECURSIVE.

\(fn BTN &optional RECURSIVE)" nil nil)

(autoload 'treemacs--goto-tag "treemacs-tags" "\
Go to the tag at BTN.

\(fn BTN)" nil nil)

(autoload 'treemacs--create-imenu-index-function "treemacs-tags" "\
The `imenu-create-index-function' for treemacs buffers." nil nil)

(function-put 'treemacs--create-imenu-index-function 'side-effect-free 't)

(register-definition-prefixes "treemacs-tags" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-themes" "treemacs-themes.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from treemacs-themes.el

(register-definition-prefixes "treemacs-themes" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-treelib" "treemacs-treelib.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from treemacs-treelib.el

(register-definition-prefixes "treemacs-treelib" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-visuals" "treemacs-visuals.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from treemacs-visuals.el

(register-definition-prefixes "treemacs-visuals" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-workspaces" "treemacs-workspaces.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-workspaces.el

(register-definition-prefixes "treemacs-workspaces" '("treemacs-"))

;;;***

;;;### (autoloads nil nil ("treemacs-faces.el" "treemacs-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; treemacs-autoloads.el ends here
