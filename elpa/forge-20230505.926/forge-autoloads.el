;;; forge-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "forge" "forge.el" (0 0 0 0))
;;; Generated autoloads from forge.el

(defvar forge-add-default-bindings t "\
Whether to add Forge's bindings to various Magit keymaps.

If you want to disable this, then you must set this to nil before
`magit' is loaded.  If you do it before `forge' but after `magit'
is loaded, then `magit-mode-map' ends up being modified anyway.

If this is nil, then `forge-toggle-display-in-status-buffer' can
no longer do its job.  It might be better to set the global value
of `forge-display-in-status-buffer' to nil instead.  That way you
can still display topics on demand in the status buffer.")

(with-eval-after-load 'git-commit (when forge-add-default-bindings (define-key git-commit-mode-map (kbd "C-c C-v") #'forge-visit-topic)))

(with-eval-after-load 'magit-mode (when forge-add-default-bindings (define-key magit-mode-map "'" #'forge-dispatch) (define-key magit-mode-map "N" #'forge-dispatch)))

(register-definition-prefixes "forge" '("forge-"))

;;;***

;;;### (autoloads nil "forge-bitbucket" "forge-bitbucket.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from forge-bitbucket.el

(register-definition-prefixes "forge-bitbucket" '("forge-bitbucket-repository"))

;;;***

;;;### (autoloads nil "forge-commands" "forge-commands.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from forge-commands.el
 (autoload 'forge-dispatch "forge-commands" nil t)

(autoload 'forge-pull "forge-commands" "\
Pull topics from the forge repository.

With a prefix argument and if the repository has not been fetched
before, then read a date from the user and limit pulled topics to
those that have been updated since then.

If pulling is too slow, then also consider setting the Git variable
`forge.omitExpensive' to `true'.

\(fn &optional REPO UNTIL)" t nil)

(autoload 'forge-pull-notifications "forge-commands" "\
Fetch notifications for all repositories from the current forge." t nil)

(autoload 'forge-pull-topic "forge-commands" "\
Pull the API data for the current topic.
If there is no current topic or with a prefix argument read a
TOPIC to pull instead.

\(fn TOPIC)" t nil)

(autoload 'forge-browse-dwim "forge-commands" "\
Visit a topic, branch or commit using a browser.
Prefer a topic over a branch and that over a commit." t nil)

(autoload 'forge-browse-commit "forge-commands" "\
Visit the url corresponding to REV using a browser.

\(fn REV)" t nil)

(autoload 'forge-copy-url-at-point-as-kill "forge-commands" "\
Copy the url of the thing at point." t nil)

(autoload 'forge-browse-branch "forge-commands" "\
Visit the url corresponding BRANCH using a browser.

\(fn BRANCH)" t nil)

(autoload 'forge-browse-remote "forge-commands" "\
Visit the url corresponding to REMOTE using a browser.

\(fn REMOTE)" t nil)

(autoload 'forge-browse-repository "forge-commands" "\
View the current repository in a separate buffer.

\(fn REPO)" t nil)

(autoload 'forge-browse-topic "forge-commands" "\
Visit the current topic using a browser." t nil)

(autoload 'forge-browse-pullreqs "forge-commands" "\
Visit the pull-requests of the current repository using a browser." t nil)

(autoload 'forge-browse-pullreq "forge-commands" "\
Visit the url corresponding to PULLREQ using a browser.

\(fn PULLREQ)" t nil)

(autoload 'forge-browse-issues "forge-commands" "\
Visit the issues of the current repository using a browser." t nil)

(autoload 'forge-browse-issue "forge-commands" "\
Visit the current issue using a browser.
If there is no current issue or with a prefix argument
read an ISSUE to visit.

\(fn ISSUE)" t nil)

(autoload 'forge-browse-post "forge-commands" "\
Visit the current post using a browser." t nil)

(autoload 'forge-visit-topic "forge-commands" "\
View the current topic in a separate buffer.
If there is no current topic or with a prefix argument
read a topic to visit instead.

\(fn TOPIC)" t nil)

(autoload 'forge-visit-pullreq "forge-commands" "\
View the current pull-request in a separate buffer.
If there is no current pull-request or with a prefix argument
read a PULLREQ to visit instead. If point is looking at a pullreq
reference with Gitlab/Github notation try to visit the pullreq
with that number.

\(fn PULLREQ)" t nil)

(autoload 'forge-visit-issue "forge-commands" "\
Visit the current issue in a separate buffer.
If there is no current issue or with a prefix argument read an
ISSUE to visit instead. If point is looking at an issue reference
with Gitlab/Github notation try to visit the issue with that
number.

\(fn ISSUE)" t nil)

(autoload 'forge-visit-repository "forge-commands" "\
View the current repository in a separate buffer.

\(fn REPO)" t nil)

(autoload 'forge-branch-pullreq "forge-commands" "\
Create and configure a new branch from a pull-request.
Please see the manual for more information.

\(fn PULLREQ)" t nil)

(autoload 'forge-checkout-pullreq "forge-commands" "\
Create, configure and checkout a new branch from a pull-request.
Please see the manual for more information.

\(fn PULLREQ)" t nil)

(autoload 'forge-checkout-worktree "forge-commands" "\
Create, configure and checkout a new worktree from a pull-request.
This is like `forge-checkout-pullreq', except that it also
creates a new worktree. Please see the manual for more
information.

\(fn PATH PULLREQ)" t nil)

(autoload 'forge-fork "forge-commands" "\
Fork the current repository to FORK and add it as a REMOTE.
If the fork already exists, then that isn't an error; the remote
is added anyway.  Currently this only supports Github and Gitlab.

\(fn FORK REMOTE)" t nil)

(autoload 'forge-list-notifications "forge-commands" "\
List notifications." t nil)

(autoload 'forge-add-pullreq-refspec "forge-commands" "\
Configure Git to fetch all pull-requests.
This is done by adding \"+refs/pull/*/head:refs/pullreqs/*\"
to the value of `remote.REMOTE.fetch', where REMOTE is the
upstream remote.  Also fetch from REMOTE." t nil)

(autoload 'forge-add-repository "forge-commands" "\
Add a repository to the database.
Offer to either pull topics (now and in the future) or to only
pull individual topics when the user invokes `forge-pull-topic'.

\(fn URL)" t nil)

(function-put 'forge-add-repository 'interactive-only 't)

(autoload 'forge-add-user-repositories "forge-commands" "\
Add all of USER's repositories from HOST to the database.
This may take a while.  Only Github is supported at the moment.

\(fn HOST USER)" t nil)

(autoload 'forge-add-organization-repositories "forge-commands" "\
Add all of ORGANIZATION's repositories from HOST to the database.
This may take a while.  Only Github is supported at the moment.

\(fn HOST ORGANIZATION)" t nil)

(autoload 'forge-merge "forge-commands" "\
Merge the current pull-request using METHOD using the forge's API.

If there is no current pull-request or with a prefix argument,
then read pull-request PULLREQ to visit instead.

Use of this command is discouraged.  Unless the remote repository
is configured to disallow that, you should instead merge locally
and then push the target branch.  Forges detect that you have
done that and respond by automatically marking the pull-request
as merged.

\(fn PULLREQ METHOD)" t nil)

(autoload 'forge-remove-repository "forge-commands" "\
Remove a repository from the database.

\(fn HOST OWNER NAME)" t nil)

(autoload 'forge-remove-topic-locally "forge-commands" "\
Remove a topic from the local database only.
Due to how the supported APIs work, it would be too expensive to
automatically remove topics from the local database that were
removed from the forge.  The purpose of this command is to allow
you to manually clean up the local database.

\(fn TOPIC)" t nil)

(autoload 'forge-rename-default-branch "forge-commands" "\
Rename the default branch to NEWNAME.
Change the name on the upstream remote and locally, and update
the upstream remotes of local branches accordingly." t nil)

(autoload 'forge-reset-database "forge-commands" "\
Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development." t nil)

(register-definition-prefixes "forge-commands" '("forge-"))

;;;***

;;;### (autoloads nil "forge-core" "forge-core.el" (0 0 0 0))
;;; Generated autoloads from forge-core.el

(register-definition-prefixes "forge-core" '("forge-"))

;;;***

;;;### (autoloads nil "forge-db" "forge-db.el" (0 0 0 0))
;;; Generated autoloads from forge-db.el

(register-definition-prefixes "forge-db" '("forge-"))

;;;***

;;;### (autoloads nil "forge-gitea" "forge-gitea.el" (0 0 0 0))
;;; Generated autoloads from forge-gitea.el

(register-definition-prefixes "forge-gitea" '("forge-gitea-repository"))

;;;***

;;;### (autoloads nil "forge-github" "forge-github.el" (0 0 0 0))
;;; Generated autoloads from forge-github.el

(register-definition-prefixes "forge-github" '("forge-"))

;;;***

;;;### (autoloads nil "forge-gitlab" "forge-gitlab.el" (0 0 0 0))
;;; Generated autoloads from forge-gitlab.el

(register-definition-prefixes "forge-gitlab" '("forge-gitlab-repository"))

;;;***

;;;### (autoloads nil "forge-gogs" "forge-gogs.el" (0 0 0 0))
;;; Generated autoloads from forge-gogs.el

(register-definition-prefixes "forge-gogs" '("forge-gogs-repository"))

;;;***

;;;### (autoloads nil "forge-issue" "forge-issue.el" (0 0 0 0))
;;; Generated autoloads from forge-issue.el

(register-definition-prefixes "forge-issue" '("forge-"))

;;;***

;;;### (autoloads nil "forge-list" "forge-list.el" (0 0 0 0))
;;; Generated autoloads from forge-list.el

(autoload 'forge-list-topics "forge-list" "\
List topics of the current repository in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-issues "forge-list" "\
List issues of the current repository in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-labeled-issues "forge-list" "\
List issues of the current repository that have LABEL.
List them in a separate buffer.

\(fn ID LABEL)" t nil)

(autoload 'forge-list-assigned-issues "forge-list" "\
List issues of the current repository that are assigned to you.
List them in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-owned-issues "forge-list" "\
List open issues from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now." t nil)

(autoload 'forge-list-pullreqs "forge-list" "\
List pull-requests of the current repository in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-labeled-pullreqs "forge-list" "\
List pull-requests of the current repository that have LABEL.
List them in a separate buffer.

\(fn ID LABEL)" t nil)

(autoload 'forge-list-assigned-pullreqs "forge-list" "\
List pull-requests of the current repository that are assigned to you.
List them in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-requested-reviews "forge-list" "\
List pull-requests of the current repository that are awaiting your review.
List them in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-owned-pullreqs "forge-list" "\
List open pull-requests from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now." t nil)

(autoload 'forge-list-authored-pullreqs "forge-list" "\
List open pull-requests of the current repository that are authored by you.
List them in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-authored-issues "forge-list" "\
List open issues from the current repository that are authored by you.
List them in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-repositories "forge-list" "\
List known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database." t nil)

(autoload 'forge-list-owned-repositories "forge-list" "\
List your own known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database
and options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now." t nil)

(register-definition-prefixes "forge-list" '("forge-"))

;;;***

;;;### (autoloads nil "forge-notify" "forge-notify.el" (0 0 0 0))
;;; Generated autoloads from forge-notify.el

(register-definition-prefixes "forge-notify" '("forge-"))

;;;***

;;;### (autoloads nil "forge-post" "forge-post.el" (0 0 0 0))
;;; Generated autoloads from forge-post.el

(register-definition-prefixes "forge-post" '("forge-"))

;;;***

;;;### (autoloads nil "forge-pullreq" "forge-pullreq.el" (0 0 0 0))
;;; Generated autoloads from forge-pullreq.el

(register-definition-prefixes "forge-pullreq" '("forge-"))

;;;***

;;;### (autoloads nil "forge-repo" "forge-repo.el" (0 0 0 0))
;;; Generated autoloads from forge-repo.el

(register-definition-prefixes "forge-repo" '("forge-"))

;;;***

;;;### (autoloads nil "forge-revnote" "forge-revnote.el" (0 0 0 0))
;;; Generated autoloads from forge-revnote.el

(register-definition-prefixes "forge-revnote" '("forge-revnote"))

;;;***

;;;### (autoloads nil "forge-semi" "forge-semi.el" (0 0 0 0))
;;; Generated autoloads from forge-semi.el

(register-definition-prefixes "forge-semi" '("forge-"))

;;;***

;;;### (autoloads nil "forge-topic" "forge-topic.el" (0 0 0 0))
;;; Generated autoloads from forge-topic.el

(register-definition-prefixes "forge-topic" '("forge-"))

;;;***

;;;### (autoloads nil nil ("forge-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; forge-autoloads.el ends here
