;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;; Code:

(disable-packages! helm
                   helm-core
                   solaire-mode
                   anaconda-mode
                   company-anaconda
                   lsp-python-ms
                   pyimport)

;; (package! all-the-icons-dired)
(package! ansible)
;; (package! asana
;;   :recipe (:host github :repo "lmartel/emacs-asana"))
(package! async)
;; (package! auto-complete)
(package! auto-yasnippet)
(package! babel)
(package! bazel-mode :recipe
  (:host github
   :repo "codesuki/bazel-mode"
   :files ("bazel-mode.el")))
(package! bazel :recipe
  (:host github
   :repo "bazelbuild/emacs-bazel-mode"
   :files ("lisp/*.el")))
(package! better-shell)
;; (package! bind-key)
;; (package! bts)
;; (package! bts-github)
(package! company)
;; (package! company-ansible)
;; (package! company-c-headers)
(package! company-go)
;; (package! company-jedi)
(package! company-irony)
(package! company-lsp)
(package! company-quickhelp)
(package! company-shell)
(package! company-terraform)
(package! copy-as-format)
;; (package! concurrent)
(package! counsel-etags)
(package! counsel-ghq
  :recipe (:host github :repo "windymelt/counsel-ghq"))
(package! counsel-osx-app)
(package! counsel-projectile)
(package! counsel-tramp)
(package! csv-mode)
;; (package! ctable)
;; (package! dash)
;; (package! dash-functional)
;; (package! dashboard)
;; (package! dashboard-project-status)
;; (package! deferred)
;; (package! deft)
;; (package! diminish)
;; (package! docker)
;; (package! docker-compose-mode)
;; (package! docker-tramp)
(package! dockerfile-mode)
;; (package! drag-stuff)
(package! dumb-jump)
(package! el-pocket)
;; (package! eldoc-overlay)
;; (package! elpy)
;; (package! epc)
;; (package! epl)
;; (package! exec-path-from-shell)
;; (package! expand-region)
;; (package! f)
;; (package! flycheck)
;; (package! flycheck-bazel :git https://github.com/michaelschiff/flycheck-bazel.git)
;; (package! flycheck-golangci-lint)
(package! flycheck-mypy)
;; (package! flycheck-popup-tip)
;; (package! flymake-easy)
(package! flymake-go)
;; (package! flymake-json)
(package! flymake-python-pyflakes)
(package! flymake-yaml)
(package! fzf)
;; (package! ggtags)
;; (package! gh)
;; (package! ghub)
;; (package! ghub+)
;; (package! gist)
;; (package! git)
;; (package! git-commit)
;; (package! git-gutter)
;; (package! git-gutter+)
(package! git-link)
(package! github-browse-file)
(package! github-issues)
(package! github-pullrequest)
;; (package! github-review)
;; (package! github-search)
;; (package! github-stars)
;; (package! gitignore-mode)
(package! go-eldoc)
(package! go-gopath)
(package! go-mode)
(package! go-projectile)
(package! go-snippets)
;; (package! golden-ratio)
;; (package! graphene)
;; (package! graphene-meta-theme)
;; (package! graphql)
;; (package! guess-style :git https://github.com/nschum/guess-style.git)
(package! hcl-mode)
;; (package! hide-mode-line)
;; (package! highlight-symbol)
;; (package! ht)
;; (package! idle-highlight-mode)
;; (package! indent-guide)
;; (package! initchart :git https://github.com/yuttie/initchart.git)
(package! ivy-ghq :recipe
  (:host github
         :repo "analyticd/ivy-ghq"
         :files ("*.el")))
(package! ivy-rich)
;; (package! jade-mode)
;; (package! jinja2-mode)
;; (package! json-mode)
;; (package! json-reformat)
;; (package! json-snatcher)
(package! k8s-mode)
;; (package! kubel)
(package! kubernetes)
(package! kubernetes-tramp)
(package! lsp-ivy)
(package! lsp-mode)
;; (package! lsp-python)
;; (package! lsp-python-ms)
;; (package! lsp-sh)
;; (package! lsp-typescript)
;; (package! lsp-ui)
;; (package! lsp-yaml :git https://github.com/iquiw/lsp-yaml.git)
;; (package! magit)
;; (package! magit-popup)
(package! magithub)
(package! markdown-mode)
;;(package! markdown-mode+)
;; (package! markdown-preview-eww)
;; (package! markdown-preview-mode)
;; (package! marshal)
(package! mmm-mode)
;;(package! multi-vterm)
;; (package! multiple-cursors)
(package! nginx-mode)
(package! ob-async)
(package! ob-go)
(package! ob-typescript)
;; (package! orca)
(package! org)
(package! org-autolist)
(package! org-beautify-theme)
(package! org-bullets)
(package! org-dashboard)
;; (package! org-doing)
(package! org-gcal)
(package! org-github-issues
  :recipe (:host github :repo "iensu/org-github-issues"))
(package! org-jira)
(package! org-msg)
(package! org-notebook)
;; (package! org-panes :git https://github.com/knupfer/org-panes.git)
(package! org-projectile)
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))
(package! company-org-roam
  :recipe (:host github :repo "org-roam/company-org-roam"))
(package! org-sync)
(package! org-web-tools)
(package! org2issue)
(package! org2jekyll)
(package! ox-gfm)
;; (package! package-build)
(package! pbcopy)
;; (package! pcache)
;; (package! pkg-info)
(package! pocket-mode)
;; (package! pocket-reader)
;; (package! popup)
;; (package! popwin)
;; (package! pos-tip)
;; (package! ppd-sr-speedbar)
;; (package! prodigy)
;; (package! project-persist)
;; (package! project-persist-drawer)
;; (package! projectile)
(package! py-isort)
(package! pyenv-mode-auto)
(package! python-black)
;; (package! python-environment)
;; (package! python-mode)
;; (package! rainbow-delimiters)
;; (package! recentf-ext)
;; (package! s)
(package! sbt-mode)
(package! selectrum)
;; (package! sequential-command)
;; (package! shut-up)
;; (package! smart-newline)
;; (package! smartparens)
;; (package! smartrep)
;; (package! smex)
;; (package! sr-speedbar)
;; (package! ssh-config-mode)
(package! swiper)
(package! terraform-mode)
(package! tramp-term)
;; (package! transient)
;; (package! tree-mode)
(package! typescript-mode)
(package! vagrant-tramp)
;; (package! web-mode)
;; (package! wgrep)
(package! which-key)
;; (package! with-editor)
(package! whitespace)
;; (package! yaml-mode)
(package! yasnippet)
;; (package! yaxception)

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.

;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(provide 'packages)
;;; packages.el ends here
