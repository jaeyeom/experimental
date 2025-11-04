;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   (append
    '(
      ;; ----------------------------------------------------------------
      ;; Example of useful layers you may want to use right away.
      ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
      ;; `M-m f e R' (Emacs style) to install them.
      ;; ----------------------------------------------------------------
      ansible
      auto-completion
      better-defaults
      (c-c++ :variables
             c-c++-default-mode-for-headers 'c++-mode
             c-c++-enable-clang-support t
             c-c++-enable-clang-format-on-save t)
      (compleseus :variables
                  compleseus-consult-preview-keys '(:debounce 1.0 any))
      copy-as-format
      csv
      (dart :variables
            lsp-dart-sdk-dir "~/flutter/bin/cache/dart-sdk/"
            lsp-enable-on-type-formatting t)
      dtrt-indent
      emacs-lisp
      epub
      eww
      git
      github-copilot
      (go :variables
          go-backend 'lsp
          go-use-golangci-lint t
          lsp-go-use-gofumpt t)
      ;; (groovy :variables
      ;;         groovy-indent-offset 2)
      ;; (helm :variables
      ;;       ;; Prevent helm from taking over the entire frame.
      ;;       helm-show-completion-display-function
      ;;       #'helm-show-completion-default-display-function)
      html
      ;; ivy
      (javascript :variables
                  js2-basic-offset 2
                  js2-indent-level 2
                  js-indent-level 2
                  js2-strict-missing-semi-warning nil)
      (llm-client :variables
                  llm-client-enable-gptel t)
      lsp
      markdown
      multiple-cursors
      (notmuch :variables
               notmuch-spacemacs-layout-name "@Notmuch"
               notmuch-spacemacs-layout-binding "n")
      openai
      (org :variables
           org-directory (file-truename "~/Documents/projects")
           org-default-notes-file (concat org-directory "/todo.org")
           org-agenda-files (let ((agenda-dirs
                                   '("~/Documents/projects"))
                                  (agenda-files
                                   '("~/go/src/github.com/jaeyeom/experimental/devtools/setup-dev/ansible/README.org")))
                              (append
                               (apply 'append
                                      (mapcar (lambda (dir)
                                                (let ((expanded (expand-file-name dir)))
                                                  (when (file-exists-p expanded)
                                                    (directory-files-recursively (file-truename expanded)
                                                                                 "\\.org$"))))
                                              agenda-dirs))
                               (seq-filter 'file-exists-p
                                           (mapcar 'expand-file-name agenda-files))))
           org-enable-github-support t
           org-enable-roam-support t
           org-enable-roam-ui t
           org-roam-directory (file-truename "~/Documents/roam/")
           org-html-htmlize-output-type 'css
           org-startup-folded 'nofold)
      pass
      prodigy
      protobuf
      (python :variables
              python-format-on-save t)
      react
      reddit
      ;; renpy
      restclient
      ;; ruby
      rust
      search-engine
      (shell :variables
             shell-default-full-span nil
             shell-default-shell 'eshell)
      shell-scripts
      slack
      (spell-checking :variables
                      spell-checking-enable-by-default nil
                      spell-checking-enable-auto-dictionary t)
      sql
      syntax-checking
      ;; terraform
      toml
      treemacs
      (typescript :variables
                  typescript-fmt-on-save t
                  typescript-fmt-tool 'typescript-formatter)
      (unicode-fonts :variables
                     unicode-fonts-enable-ligatures t)
      version-control
      yaml
      )
    (if (executable-find "docker")
        '(docker))
    (if (display-graphic-p)
        '(eaf))
    (if (executable-find "w3m")
        '(w3m))
    )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     atomic-chrome
     bazel
     chatgpt-shell
     (claude-code :location (recipe :fetcher github
                                    :repo "stevemolitor/claude-code.el"
                                    :files ("*.el" (:exclude "demo.gif"))))
     ;;; Temporarily disable consult-gh-* package because it depends on Emacs 30
     ;;; which isn't released yet.
     ;; consult-gh
     ;; consult-gh-embark
     ;; consult-gh-forge
     copilot-chat
     cov
     dirvish
     eshell-command-not-found
     green-is-the-new-black-theme
     (highlight-chars :location (recipe :fetcher github
                                        :repo "emacsmirror/highlight-chars"
                                        :files ("*.el")))
     ob-async
     ob-chatgpt-shell
     ob-go
     ob-mermaid
     ob-tmux
     org-tree-slide
     ox-clip
     solarized-theme
     (tramp-gh :location (recipe :fetcher github :repo "jaeyeom/tramp-gh"))
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   (append
    '()
    ;; `emoji-chat-sheet-plus' causes a PNG error in terminal mode. But this is
    ;; loaded by Spacemacs by default. So, let's exclude it.
    (unless (display-graphic-p)
      '(emoji-cheat-sheet-plus))
    )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "nerd-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light). A theme from external
   ;; package can be defined with `:package', or a theme can be defined with
   ;; `:location' to download the theme package, refer the themes section in
   ;; DOCUMENTATION.org for the full theme specifications.
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '(("FiraCode Nerd Font Mono" :size 10.0 :weight normal :width normal)
                               ("FiraCode Nerd Font" :size 10.0 :weight normal :width normal)
                               ("Fira Code" :size 10.0 :weight normal :width normal)
                               ("SF Mono" :size 10.0 :weight normal :width normal)
                               ("Cousine" :size 10.0 :weight normal :width normal)
                               ("DejaVu Sans Mono" :size 10.0 :weight normal :width normal))

   ;; Default icons font, it can be `all-the-icons' or `nerd-icons'.
   dotspacemacs-default-icons-font 'all-the-icons

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "M-<return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "M-<return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'visual

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `ack' and `grep'.
   ;; (default '("rg" "ag" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "ack" "grep")

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (defun my/crostini-p ()
    (file-exists-p "/dev/.cros_milestone"))

  (defvar my/crostini-p (my/crostini-p))

  (defun my/termux-p ()
    (not (null (getenv "TERMUX_VERSION"))))

  (defvar my/termux-p (my/termux-p))

  (defun my/macos-p ()
    (eq system-type 'darwin))

  (defvar my/macos-p (my/macos-p))

  ;; Bug patch for Termux. `async-start' does not work properly in Termux. Let's
  ;; make the function synchronous.
  (if my/termux-p
      (with-eval-after-load 'async
        (defun async-start (start-func &optional finish-func)
          "Different from the original function, this function is synchronous."
          (let ((result (funcall start-func)))
            (when finish-func
              (funcall finish-func result))
            result))))

  (customize-set-variable
   'custom-file (expand-file-name ".spacemacs-custom.el" (or dotspacemacs-directory "~/")) "Separate custom file")
  (load custom-file)
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;;; Basic

  ;; Somehow Spacemacs sets the default tab width to 2. Let's revert it back.
  (setopt tab-width 8)

  ;;; Get user full name and mail from git config
  (setopt user-full-name (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.name"))
          user-mail-address (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.email")))
  (setq-default github-username (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get github.user")))

  ;;; Authentication
  (setopt epg-pinentry-mode 'loopback)

  ;;; Enable auth source pass
  (with-eval-after-load 'auth-source-pass
    (auth-source-pass-enable)
    (setopt auth-sources '(password-store))
    (defun my/auth-source-pass-entries (host)
      (seq-filter (lambda (entry-data) (equal `("host" . ,host) (assoc "host" entry-data)))
                  (mapcar 'auth-source-pass-parse-entry (auth-source-pass-entries)))))

  (require 'auth-source-pass nil 'noerror)

  ;;; Text Mode
  (add-hook 'text-mode-hook 'turn-on-auto-fill)

  ;;; Termux Permission denied solution
  (when my/termux-p
    (defvar my/directory-fallback-alist
      '(("/" . ("data"))
        ("/data/" . ("data"))
        ("/data/data/" . ("com.termux")))
      "An alist mapping specific directories to fallback file lists which should have / at the end.")

    (defun my/directory-files-advice (orig-fun directory &optional full match nosort count)
      "Safely list files in DIRECTORY, handling special cases defined in `my/directory-fallback-alist`.
Replicates the behavior of `directory-files` with FULL, MATCH, NOSORT, and COUNT arguments.
Fallback file lists are returned for specific directories."

      ;; Lookup fallback for the given directory
      (let* ((expanded-directory (file-name-as-directory (expand-file-name directory)))
             (found (assoc-default expanded-directory my/directory-fallback-alist #'string-equal))
             (fallback (and found (append '("." "..") found))))
        (if fallback
            ;; Handle fallback case
            (let* ((files (if match
                              ;; If MATCH is provided, filter fallback files
                              (seq-filter (lambda (f) (string-match-p match f)) fallback)
                            fallback)) ;; Otherwise, return full fallback list
                   (paths (if full
                              ;; If FULL is non-nil, prepend EXPANDED-DIRECTORY to each file
                              (mapcar (lambda (f) (concat expanded-directory f)) files)
                            files))
                   (sorted-paths (if nosort
                                     paths
                                   (sort paths #'string-lessp))))
              (if count
                  (seq-take sorted-paths count)
                sorted-paths))
          ;; If no fallback, call original directory-files
          (funcall orig-fun directory full match nosort count))))

    (advice-add 'directory-files :around #'my/directory-files-advice))

  ;;; Helm
  (if my/termux-p
      (with-eval-after-load 'helm
        ;; Termux locate does not support -N option.
        (setopt helm-locate-command "locate %s -e -A --regex %s")))

  ;;; Compleseus
  (defun my/minibuffer-up-directory ()
    "Navigate to the parent directory in the minibuffer during file completion."
    (interactive)
    (let ((parent (file-name-directory (directory-file-name (minibuffer-contents)))))
      (when parent
        (delete-minibuffer-contents)
        (insert parent))))

  (with-eval-after-load 'minibuffer
    (define-key minibuffer-local-filename-completion-map
                (kbd "<backtab>") #'my/minibuffer-up-directory))

  ;; Vertico does not take minibuffer-local-filename-completion-map into
  ;; account. So this "<backtab>" may trigger weird behavior in non-file
  ;; context.
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "<backtab>") #'my/minibuffer-up-directory))

  (with-eval-after-load 'orderless
    ;; Revert to the default. Spacemacs sets this with "[ &]", which hinders "&"
    ;; for annotation search.
    (setopt orderless-component-separator #'orderless-escapable-split-on-space))

  ;;; Eat
  (with-eval-after-load 'eat
    (setopt eat-term-name "xterm-256color")
    (add-hook 'eshell-load-hook #'eat-eshell-mode)
    (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

  ;;; Set up buildifier for bazel
  (with-eval-after-load 'bazel
    (add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-buildifier nil t))))

  ;; Bazel jump improvement
  (defun my/switch-to-truename ()
    "Switch current buffer to the true filepath."
    (interactive)
    (let ((true-path (file-truename (buffer-file-name))))
      (find-alternate-file true-path)))

  (defun my/compilation-find-file (orig-fun marker filename directory &rest args)
    "Advice function to resolve true file paths in compilation mode."
    (let* ((true-filename (file-truename filename))
           (true-directory (file-name-directory true-filename)))
      (apply orig-fun marker true-filename true-directory args)))

  (advice-add 'compilation-find-file :around #'my/compilation-find-file)

  ;;; Font Settings

  ;; Fontconfig almost never works. The following tries to find the best font on
  ;; the system. Due to Korean character range settings, it's not possible to
  ;; use a single perfect setting. Some switching is necessary until a better
  ;; solution is found.
  (when (display-graphic-p)
    ;; Font Helpers
    (defun my/first-font-available (fonts)
      "Return the first available font from the list of FONTS or nil."
      (seq-find (lambda (font) (find-font (font-spec :family font))) fonts))

    (defvar my/default-font-hangul
      (my/first-font-available
       '("Noto Sans CJK KR"
         "Sarasa Mono TC Nerd Font"
         "UnDotum")))

    (defvar my/reading-font
      (my/first-font-available
       '("TeX Gyre Bonum"
         "Noto Serif"
         "Serif")))

    (defvar my/reading-font-hangul
      (my/first-font-available
       '("Noto Serif CJK KR"
         "UnBatang"
         "Serif")))

    (defvar my/true-monospace-font
      (my/first-font-available
       '("D2Coding ligature"
         "Sarasa Mono TC Nerd Font"
         "NanumGothicCoding")))

    (defun my/set-default-font ()
      "Set the default font to `my/default-font' and `my/default-font-hangul'."
      (interactive)
      (spacemacs/set-default-font dotspacemacs-default-font)
      (set-fontset-font t 'hangul (font-spec :family my/default-font-hangul)))

    (defun my/set-reading-font ()
      "Set the reading font to `my/reading-font' and `my/reading-font-hangul'."
      (interactive)
      (set-face-attribute 'variable-pitch nil :family my/reading-font :height 1.0)
      (set-face-attribute 'variable-pitch-text nil :family my/reading-font :height 1.0)
      (set-fontset-font t 'hangul (font-spec :family my/reading-font-hangul)))

    (defun my/set-true-monospace-font ()
      "Set the true monospace font to `my/true-monospace-font'."
      (interactive)
      (set-face-attribute 'default nil :family my/true-monospace-font :height 110)
      (set-fontset-font t 'hangul (font-spec :family my/true-monospace-font)))

    ;; ========== nov.el (EPUB) Override ==========
    (with-eval-after-load 'nov
      (setopt nov-text-width 80)

      (defun my/nov-font-setup ()
        (my/set-reading-font))

      (add-hook 'nov-mode-hook #'my/nov-font-setup))

    )

  (with-eval-after-load 'highlight-chars
    ;; Visually confusing non-standard characters.
    (setf hc-other-chars '("\u2018\u2019\u201C\u201D\u2013\u2014\u2026\u2192\u2190\u2194\u202F\xA0\xAD\u200C\u200D"))
    (add-hook 'prog-mode-hook #'hc-highlight-other-chars))

  (require 'highlight-chars nil 'noerror)

  ;;; Reddit
  (with-eval-after-load 'reddigg
    (defun my/reddigg-view-comments-no-query-string (orig-fun cmt &rest args)
      "Remove query string from the url for `reddigg-view-comments'."
      (let ((cmt (car (split-string cmt "\\?"))))
        (apply orig-fun cmt args)))

    (advice-add 'reddigg-view-comments :around #'my/reddigg-view-comments-no-query-string)

    (defun reddigg-find-browse-url-function (url)
      "Return the right reddigg function to browse URL and args. It
returns `nil' if no reddigg function is found. This function can
be used for predicate for `browse-url-default-handlers'.'"
      (let* ((parsed-url (url-generic-parse-url url))
             (host (url-host parsed-url))
             (path (car (split-string (url-filename parsed-url) "\\?"))))
        (if (or (string= host "www.reddit.com")
                (string= host "reddit.com"))
            (cond ((string-match "^/r/[^/]+/comments/" path)
                   (list 'reddigg-view-comments url))
                  ((string-match "^/r/\\([^/]+\\)/?$" path)
                   (list 'reddigg-view-sub (match-string 1 path)))))))

    (defun reddigg-browse-url (url &rest args)
      "Browse reddit URL using reddigg.
If URL is comments page then use `reddigg-view-comments' to browse the URL.
If URL is subreddit page then use `reddigg-view-sub' to browse the URL."
      (interactive (browse-url-interactive-arg "URL: "))
      (let ((fn (reddigg-find-browse-url-function url)))
        (if fn
            (apply (car fn) (cdr fn))
          (message "No reddigg function found for %s" url)))))

  (add-to-list 'browse-url-handlers
               '(reddigg-find-browse-url-function . reddigg-browse-url))

  (autoload 'reddigg-find-browse-url-function "reddigg")

  ;; Workaround for Termux for iamge display errors.
  (when my/termux-p
    (defun my/skip-image-display-advice (orig-fun &rest args)
      (if (display-images-p)
          (apply orig-fun args)
        (message "Image display not available. Skipping inline images. Args: %S" args)))

    (with-eval-after-load 'markdown-mode
      (advice-add 'markdown-display-inline-images :around #'my/skip-image-display-advice)))

  ;; Make termux-open work with browse-url.
  (if my/termux-p
      (with-eval-after-load 'browse-url
        ;; Define Android browser function to use xdg-open (which will be termux-open)
        (defun browse-url-default-android-browser (url &rest args)
          "Open URL using xdg-open (termux-open) on Android."
          (browse-url-xdg-open url args))

        (defun my/browse-url-can-use-xdg-open-advice (orig-fun &rest args)
          "Use termux-open if available."
          (if (executable-find "termux-open")
              t
            (apply orig-fun args)))

        (advice-add 'browse-url-can-use-xdg-open :around #'my/browse-url-can-use-xdg-open-advice)))

  (with-eval-after-load 'eww
    ;; To open external browser from eww, press `, v x'
    (setopt browse-url-browser-function 'eww
            browse-url-secondary-browser-function 'browse-url-default-browser)

    ;; Use browse-url-handlers for all URLs in eww. But the redirection URLs are
    ;; not working.
    (setopt eww-use-browse-url "")

    (defun eww-browse-with-browse-url (&optional url)
      "Browse the current URL with `browse-url'."
      (interactive nil eww-mode)
      (let ((browse-url-secondary-browser-function 'browse-url))
        (eww-browse-with-external-browser url)))

    ;; To browse-url from eww, press `, v u'
    (spacemacs/set-leader-keys-for-major-mode 'eww-mode
      "vu" 'eww-browse-with-browse-url)

    ;; TAB for `shr-next-link' is very convenient, but Spacemacs eww layer
    ;; defines C-i as `eww-forward-url' and that overrides tab in terminal. This
    ;; is to revert that.
    (evil-define-key 'normal eww-mode-map
      (kbd "C-i") 'shr-next-link))

  (with-eval-after-load 'shr
    ;; I do not like proportional fonts.
    (setopt shr-use-fonts nil)

    ;; I do not like too big images.
    (setopt shr-max-image-proportion 0.5)
    )

  ;;; Org Mode

  ;; Set indentation for org-export-define-derived-backend even when org isn't loaded
  (put 'org-export-define-derived-backend 'lisp-indent-function 2)

  (with-eval-after-load 'org
    ;; Org Roam
    (org-roam-db-autosync-mode)

    ;; Babel
    (require 'ob-awk nil 'noerror)
    (require 'ob-chatgpt-shell nil 'noerror)
    (require 'ob-emacs-lisp nil 'noerror)
    (require 'ob-eshell nil 'noerror)
    (require 'ob-go nil 'noerror)
    (require 'ob-makefile nil 'noerror)
    (require 'ob-mermaid nil 'noerror)
    (require 'ob-python nil 'noerror)
    (require 'ob-shell nil 'noerror)
    (require 'ob-tmux nil 'noerror)

    ;; Convenience function
    (defun my/org-surround-region-with-src-block (lang)
      "Surround the current region with an Org-mode source block of LANG."
      (interactive "sEnter language: ")
      (let ((region-start (region-beginning))
            (region-end (region-end)))
        (goto-char region-end)
        (insert (format "#+END_SRC\n"))
        (goto-char region-start)
        (insert (format "#+BEGIN_SRC %s\n" lang))))

    ;; Export Org mode with Pandoc to buffer.
    (defun my/org-export-to-gfm-markdown-buffer ()
      "Export the current buffer to GFM Markdown format using Pandoc and switch to a new buffer with the result.

This function was written because the default GFM export in Org
mode does not work with Roam links."
      (interactive)
      (let ((original-buffer (current-buffer))
            (output-buffer (generate-new-buffer "*Org to GFM*")))
        (with-current-buffer output-buffer
          (org-mode) ;; Optional: Set the mode of the new buffer if needed
          (insert-buffer-substring original-buffer)
          (call-process-region (point-min) (point-max) "pandoc" t t nil
                               "--wrap=none" "--from=org" "--to=gfm")
          (markdown-mode))
        (switch-to-buffer output-buffer)))

    (org-export-define-derived-backend 'pandoc-gfm 'gfm
      :menu-entry
      '(?g "Export to Github Flavored Markdown"
           ((?P "To pandoc temporary buffer"
                (lambda (a s v b) (my/org-export-to-gfm-markdown-buffer))))))

    ;; Export Org mode to MHTML file with inline images.
    ;;
    ;; This code was copied from https://niklasfasching.de/posts/org-html-export-inline-images/
    ;;
    ;; NOTE(jaeyeom): The naming mhtml isn't technically correct, because it's
    ;; using data URI scheme instead of MIME HTML.
    (defun my/org-html-export-to-mhtml (async subtree visible body)
      (cl-letf (((symbol-function 'org-html--format-image) 'my/format-image-inline))
        (org-html-export-to-html async subtree visible body)))

    (defun my/format-image-inline (source attributes info)
      (let* ((source-file-p (f-file-p source))
             (url-or-file (if source-file-p (expand-file-name source) source))
             (ext (file-name-extension url-or-file))
             (prefix (cond
                      ((string= "svg" ext) "data:image/svg+xml;base64,")
                      ((string= "png" ext) "data:image/png;base64,")
                      ((string= "jpg" ext) "data:image/jpeg;base64,")
                      ((string= "jpeg" ext) "data:image/jpeg;base64,")
                      (t "data:;base64,")))
             (data (with-temp-buffer
                     (if source-file-p
                         (insert-file-contents-literally url-or-file)
                       (url-insert-file-contents url-or-file))
                     (buffer-string)))
             (data-url (concat prefix (base64-encode-string data)))
             (attributes (org-combine-plists `(:src ,data-url) attributes)))
        (org-html-close-tag "img" (org-html--make-attribute-string attributes) info)))

    (org-export-define-derived-backend 'html-inline-images 'html
      :menu-entry '(?h "Export to HTML" ((?m "As MHTML file" my/org-html-export-to-mhtml)))))

  (with-eval-after-load 'ob-chatgpt-shell
    (ob-chatgpt-shell-setup))

  (with-eval-after-load 'ob-mermaid
    ;; Find the executable mmdc then fall back to local node_modules.
    (setopt ob-mermaid-cli-path
            (or (executable-find "mmdc")
                (expand-file-name "~/node_modules/.bin/mmdc"))))

  (with-eval-after-load 'ob-tmux
    ;; By default ob-tmux tries to open gnome-terminal. Let's disable it.
    (setopt org-babel-tmux-terminal
            (if my/crostini-p "x-terminal-emulator")))

  (with-eval-after-load 'sqlite-mode
    (evil-define-key 'normal sqlite-mode-map (kbd "RET") 'sqlite-mode-list-data)
    (evil-define-key 'normal sqlite-mode-map (kbd "D") 'sqlite-mode-delete))

  ;;; Gmail with gmi
  (setopt message-kill-buffer-on-exit t
          sendmail-program "gmi"
          send-mail-function 'sendmail-send-it
          message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/Mail/account.gmail")
          ;; Let gmail take care of sent mail.
          notmuch-fcc-dirs nil)

  ;;; Eshell
  (with-eval-after-load 'eshell
    (when eshell-command-not-found-command
      (eshell-command-not-found-mode 1))

    ;; Make watch open a new buffer.
    (add-to-list 'eshell-visual-commands "watch")

    ;; We don't need a pager.
    (setenv "PAGER" "cat")

    ;; Predicate filters and modifiers, copied from hamacs/ha-eshell.org
    (defun eshell-org-file-tags ()
      "Parse the eshell text at point.
Looks for parameters surrounded in single quotes. Returns a
function that takes a FILE and returns nil if the file given to
it doesn't contain the org-mode #+TAGS: entry specified."

      ;; Step 1. Parse the eshell buffer for our tag between quotes
      ;;         Make sure to move point to the end of the match:
      (if (looking-at (rx "'" (group (one-or-more (not (or ")" "'"))))"'"))
          (let* ((tag (match-string 1))
                 (reg (rx line-start
                          "#+" (optional "file") "tags:"
                          (one-or-more space)
                          (zero-or-more any)
                          (literal tag) word-end)))
            (goto-char (match-end 0))

            ;; Step 2. Return the predicate function:
            ;;         Careful when accessing the `reg' variable.
            `(lambda (file)
               (with-temp-buffer
                 (insert-file-contents file)
                 (re-search-forward ,reg nil t 1))))
        (error "The `T' predicate takes an org-mode tag value in single quotes")))

    (defun ha-eshell-add-predicates ()
      "A hook to add a `eshell-org-file-tags' predicate filter to eshell."
      (add-to-list 'eshell-predicate-alist '(?T . (eshell-org-file-tags))))

    ;; Less for Eshell, copied from hamacs/ha-eshell.org
    (defun eshell-fn-on-files (fun1 fun2 args)
      "Call FUN1 on the first element in list, ARGS.
Call FUN2 on all the rest of the elements in ARGS."
      (unless (null args)
        (let ((filenames (flatten-list args)))
          (funcall fun1 (car filenames))
          (when (cdr filenames)
            (mapcar fun2 (cdr filenames))))
        ;; Return an empty string, as the return value from `fun1'
        ;; probably isn't helpful to display in the `eshell' window.
        ""))

    (defun eshell/less (&rest files)
      "Essentially an alias to the `view-file' function."
      (eshell-fn-on-files 'view-file 'view-file-other-window files))

    ;; PCRE for Eshell, copied from hamacs/ha-eshell.org
    (defmacro prx (&rest expressions)
      "Convert the rx-compatible regular EXPRESSIONS to PCRE.
  Most shell applications accept Perl Compatible Regular Expressions."
      `(rx-let ((integer (1+ digit))
                (float   (seq integer "." integer))
                (b256    (seq (optional (or "1" "2"))
                              (regexp "[0-9]\\{1,2\\}")))
                (ipaddr  (seq b256 "." b256 "." b256 "." b256))
                (time    (seq digit (optional digit) ":" (= 2 digit) (optional ":" (= 2 digit))))
                (email   (seq (1+ (regexp "[^,< ]")) "@" (1+ (seq (1+ (any alnum "-"))) ".") (1+ alnum)))
                (date    (seq (= 2 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 4 digit)))
                (ymd     (seq (= 4 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 2 digit)))
                (uuid    (seq (= 8 hex) "-" (= 3 (seq (= 4 hex) "-")) (= 12 hex)))
                (guid    (seq uuid)))
         (rxt-elisp-to-pcre (rx ,@expressions))))

    ;; Flow or Buffer cat, copied from hamacs/ha-eshell.org
    (defun eshell-getopts (defargs args)
      "Return hash table of ARGS parsed against DEFARGS.
Where DEFARGS is an argument definition, a list of plists.
For instance:
   '((:name number :short \"n\"                 :parameter integer :default 0)
     (:name title  :short \"t\" :long \"title\" :parameter string)
     (:name debug  :short \"d\" :long \"debug\"))

If ARGS, a list of _command line parameters_ is something like:

    '(\"-d\" \"-n\" \"4\" \"--title\" \"How are that\" \"this\" \"is\" \"extra\")

The hashtable return would contain these entries:

    debug t
    number 4  ; as a number
    title \"How are that\" ; as a string
    parameters (\"this\" \"is\" \"extra\") ; as a list of strings "
      (let ((retmap    (make-hash-table))
            (short-arg (rx string-start "-" (group alnum)))
            (long-arg  (rx string-start "--" (group (1+ any)))))

        ;; Let's not pollute the Emacs name space with tiny functions, as
        ;; well as we want these functions to have access to the "somewhat
        ;; global variables", `retmap' and `defargs', we use the magical
        ;; `cl-labels' macro to define small functions:

        (cl-labels ((match-short (str defarg)
                      ;; Return t if STR matches against DEFARG's short label:
                      (and (string-match short-arg str)
                           (string= (match-string 1 str)
                                    (plist-get defarg :short))))

                    (match-long (str defarg)
                      ;; Return t if STR matches against DEFARG's long label:
                      (and (string-match long-arg str)
                           (string= (match-string 1 str)
                                    (plist-get defarg :long))))

                    (match-arg (str defarg)
                      ;; Return DEFARG if STR matches its definition (and it's a string):
                      (when (and (stringp str)
                                 (or (match-short str defarg)
                                     (match-long str defarg)))
                        defarg))

                    (find-argdef (str)
                      ;; Return entry in DEFARGS that matches STR:
                      (car (--filter (match-arg str it) defargs)))

                    (process-args (arg parm rest)
                      (when arg
                        (let* ((defarg (find-argdef arg))
                               (key    (plist-get defarg :name)))
                          (cond
                           ;; If ARG doesn't match any definition, add
                           ;; everything else to PARAMETERS key:
                           ((null defarg)
                            (puthash 'parameters (cons arg rest) retmap))

                           ((plist-get defarg :help)
                            (error (documentation (plist-get defarg :help))))

                           ;; If argument definition has a integer parameter,
                           ;; convert next entry as a number and process rest:
                           ((eq (plist-get defarg :parameter) 'integer)
                            (puthash key (string-to-number parm) retmap)
                            (process-args (cadr rest) (caddr rest) (cddr rest)))

                           ;; If argument definition has a parameter, use
                           ;; the next entry as the value and process rest:
                           ((plist-get defarg :parameter)
                            (puthash key parm retmap)
                            (process-args (cadr rest) (caddr rest) (cddr rest)))

                           ;; No parameter? Store true for its key:
                           (t
                            (puthash key t retmap)
                            (process-args (car rest) (cadr rest) (cdr rest))))))))

          (process-args (car args) (cadr args) (cdr args))
          retmap)))

    (defvar ha-eshell-ebbflow-buffername "*eshell-edit*"
      "The name of the buffer that eshell can use to store temporary input/output.")

    (defun ha-eshell-ebbflow-return ()
      "Close the ebb-flow window and return to Eshell session."
      (interactive)
      (if (and (boundp 'ha-eshell-ebbflow-return-buffer)
               (bufferp 'ha-eshell-ebbflow-return-buffer))
          (pop-to-buffer ha-eshell-ebbflow-return-buffer)
        (bury-buffer)))

    (define-minor-mode ebbflow-mode
      "Editing a flow from the Eshell ebb command, so flow can pull it back."
      :lighter " ebb"
      :keymap (let ((map (make-sparse-keymap)))
                (define-key map (kbd "C-c C-q") 'ha-eshell-ebbflow-return)
                map))

    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal ebbflow-mode-map "Q" 'ha-eshell-ebbflow-return))

    (defun eshell/flow (&rest args)
      "Output the contents of one or more buffers as a string.
Usage: flow [OPTION] [BUFFER ...]
    -h, --help           show this usage screen
    -l, --lines          output contents as a list of lines
    -w, --words          output contents as a list of space-separated elements "
      (let* ((options (eshell-getopts '((:name words  :short "w" :long "words")
                                        (:name lines  :short "l" :long "lines")
                                        (:name string :short "s" :long "string")
                                        (:name help   :short "h" :long "help"
                                               :help eshell/flow))
                                      args))
             (buffers (gethash 'parameters options))
             (content (thread-last buffers
                                   (-map 'eshell-flow-buffer-contents)
                                   (s-join "\n"))))
        (if (gethash 'help options)
            (error (documentation 'eshell/flow))

          ;; No buffer specified? Use the default buffer's contents:
          (unless buffers
            (setq content
                  (eshell-flow-buffer-contents ha-eshell-ebbflow-buffername)))

          ;; Do we need to convert the output to lines or split on words?
          (cond
           ((gethash 'words options) (split-string content))
           ((gethash 'lines options) (split-string content "\n"))
           (t                        content)))))

    (defun eshell-flow-buffer-contents (buffer-name)
      "Return the contents of BUFFER as a string."
      (when buffer-name
        (save-window-excursion
          (switch-to-buffer (get-buffer buffer-name))
          (buffer-substring-no-properties (point-min) (point-max)))))

    (defun eshell-flow-buffers (buffers)
      "Convert the list, BUFFERS, to actual buffers if given buffer names."
      (if buffers
          (--map (cond
                  ((bufferp it) it)
                  ((stringp it) (get-buffer it))
                  (t            (error (format "Illegal argument of type %s: %s\n%s"
                                               (type-of arg) it
                                               (documentation 'eshell/flow)))))
                 buffers)
        ;; No buffers given? Use the default buffer:
        (list (get-buffer ha-eshell-ebbflow-buffername))))

    (defun eshell/ebb (&rest args)
      "Insert text content into *eshell-edit* buffer, or if not text is given, the output of last command.
Usage: ebb [OPTION] [text content]
    -h, --help    show this usage screen
    -m, --mode    specify the major-mode for the *eshell-edit* buffer, e.g. json
    -n, --newline separate the text contents by newlines (this is default)
    -s, --spaces  separate the text contents by spaces, instead of newlines
    -b, --begin   add text content to the beginning of the *eshell-edit* buffer
    -e, --end     add text content to the end of *eshell-edit* buffer
    -i, --insert  add text content to *eshell-edit* at point"
      (let* ((options  (eshell-getopts '((:name insert      :short "i" :long "insert")
                                         (:name append      :short "e" :long "end")
                                         (:name prepend     :short "b" :long "begin")
                                         (:name newline     :short "n" :long "newline")
                                         (:name spaces      :short "s" :long "spaces")
                                         (:name mode-option :short "m" :long "mode" :parameter string)
                                         (:name help        :short "h" :long "help"
                                                :help eshell/ebb))
                                       args))
             (location (cond
                        ((gethash 'insert  options) :insert)
                        ((gethash 'append  options) :append)
                        ((gethash 'prepend options) :prepend)
                        (t                          :replace)))
             (params   (gethash 'parameters options)))

        (if (seq-empty-p params)
            (ha-eshell-ebb-output location)
          (ha-eshell-ebb-string location (gethash 'spaces options) params))

        ;; At this point, we are in the `ha-eshell-ebbflow-buffername', and
        ;; the buffer contains the inserted data. Did we specify a major-mode?
        (when-let ((mode-option (gethash 'mode-option options)))
          (if (s-starts-with? "js" mode-option)
              (js-json-mode)  ; Or should we just go to json-ts-mode?
            (funcall (intern (concat mode-option "-mode")))))

        ;; Flip on the minor mode-option so we can close the window later on:
        (ebbflow-mode +1)
        (goto-char (point-min)))

      nil) ; Return `nil' so that it doesn't print anything in `eshell'.

    (defun ha-eshell-ebb-switch-to-buffer (insert-location)
      "Switch to `ha-eshell-ebbflow-buffername' and get the buffer ready for new data."
      (let ((return-buffer (current-buffer)))

        (if-let ((ebbwindow (get-buffer-window ha-eshell-ebbflow-buffername)))
            (select-window ebbwindow)
          (switch-to-buffer ha-eshell-ebbflow-buffername)
          (setq-local ha-eshell-ebbflow-close-window t))

        (setq-local ha-eshell-ebbflow-return-buffer return-buffer)
        (ebbflow-mode)

        (cl-case insert-location
          (:append  (goto-char (point-max)))
          (:prepend (goto-char (point-min)))
          (:insert   nil)
          (:replace (delete-region (point-min) (point-max))))))

    (defun ha-eshell-ebb-string (insert-location space-separator-p command-results)
      "Insert the COMMAND-RESULTS into the `ha-eshell-ebbflow-buffername`.
Contents are placed based on INSERT-LOCATION and, if given, separated
by SEPARATOR (which defaults to a space)."
      (let* ((sep (if space-separator-p " " "\n"))
             (str (string-join (-flatten command-results) sep)))
        (ha-eshell-ebb-switch-to-buffer insert-location)
        (insert str)))

    (defun ha-eshell-last-output ()
      "Return contents of the last command execusion in an Eshell buffer."
      (let ((start  (save-excursion
                      (goto-char eshell-last-output-start)
                      (re-search-backward eshell-prompt-regexp)
                      (next-line)
                      (line-beginning-position)))
            (end    eshell-last-output-start))
        (buffer-substring-no-properties start end)))

    (defun ha-eshell-ebb-output (insert-location)
      "Grab output from previous eshell command, inserting it into our buffer.
Gives the INSERT-LOCATION to `ha-eshell-ebb-switch-to-buffer'."
      (let ((contents (ha-eshell-last-output)))
        (ha-eshell-ebb-switch-to-buffer insert-location)
        (insert contents)))

    ;; Last results, copied from hamacs/ha-eshell.org
    (defvar ha-eshell-output (make-ring 10)
      "A ring (looped list) storing history of eshell command output.")

    (defun ha-eshell-store-last-output ()
      "Store the output from the last eshell command.
Called after every command by connecting to the `eshell-post-command-hook'."
      (let ((output
             (buffer-substring-no-properties eshell-last-input-end eshell-last-output-start)))
        (ring-insert ha-eshell-output output)))

    (add-hook 'eshell-post-command-hook 'ha-eshell-store-last-output)

    (defun eshell/output (&rest args)
      "Return an eshell command output from its history.

The first argument is the index into the historical past, where
`0' is the most recent, `1' is the next oldest, etc.

The second argument represents the returned output:
 * `text' :: as a string
 * `list' :: as a list of elements separated by whitespace
 * `file' :: as a filename that contains the output

If the first argument is not a number, it assumes the format
to be `:text'.
"
      (let (frmt element)
        (cond
         ((> (length args) 1)  (setq frmt (cadr args)
                                     element (car args)))
         ((= (length args) 0)  (setq frmt "text"
                                     element 0))
         ((numberp (car args)) (setq frmt "text"
                                     element (car args)))
         ((= (length args) 1)  (setq frmt (car args)
                                     element 0)))

        (if-let ((results (ring-ref ha-eshell-output (or element 0))))
            (cl-case (string-to-char frmt)
              (?l     (split-string results))
              (?f     (ha-eshell-store-file-output results))
              (otherwise (s-trim results)))
          "")))

    (defun ha-eshell-store-file-output (results)
      "Writes the string, RESULTS, to a temporary file and returns that file name."
      (let ((filename (make-temp-file "ha-eshell-")))
        (with-temp-file filename
          (insert results))
        filename))

    (defvar eshell-variable-aliases-list nil "Autoloading this eshell-defined variable")
    (add-to-list 'eshell-variable-aliases-list '("$"  ha-eshell-output-text))
    (add-to-list 'eshell-variable-aliases-list '("_"  ha-eshell-output-list))
    (add-to-list 'eshell-variable-aliases-list '("OUTPUT" ha-eshell-output-file))

    (defun ha-eshell-output (format-type indices)
      "Wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
      (if indices
          (eshell/output (string-to-number (caar indices)) format-type)
        (eshell/output 0 format-type)))

    (defun ha-eshell-output-text (&optional indices &rest ignored)
      "A _text_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
      (ha-eshell-output "text" indices))

    (defun ha-eshell-output-list (&optional indices &rest ignored)
      "A _list_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
      (ha-eshell-output "list" indices))

    (defun ha-eshell-output-file (&optional indices &rest ignored)
      "A _file_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
      (ha-eshell-output "file" indices))

    ;; Termux-specific eshell configuration for shebang handling
    (when my/termux-p
      ;; Configure paths for Termux environment immediately
      (let ((termux-bin "/data/data/com.termux/files/usr/bin"))
        (setenv "PATH" (concat termux-bin ":" (getenv "PATH")))
        (push termux-bin exec-path))

      ;; Set up eshell-specific configuration when eshell loads
      (with-eval-after-load 'eshell
        ;; Add interpreter redirection for /usr/bin/env
        (add-to-list 'eshell-interpreter-alist
                     '("^/usr/bin/env$" . "/data/data/com.termux/files/usr/bin/env"))

        ;; Override script interpreter for comprehensive shebang handling
        (defun my/termux-eshell-script-interpreter (orig-fun file)
          "Handle shebang lines with Termux path corrections."
          (let ((interpreter-info (funcall orig-fun file)))
            (when interpreter-info
              (let ((interpreter (car interpreter-info))
                    (args (cdr interpreter-info)))
                (cond
                 ;; Handle /usr/bin/env specifically
                 ((string-equal interpreter "/usr/bin/env")
                  (cons "/data/data/com.termux/files/usr/bin/env" args))
                 ;; Handle other /usr/bin paths
                 ((string-match "^/usr/bin/" interpreter)
                  (cons (replace-regexp-in-string
                         "^/usr/bin/" "/data/data/com.termux/files/usr/bin/"
                         interpreter)
                        args))
                 ;; Handle /bin paths (like /bin/sh, /bin/bash)
                 ((string-match "^/bin/" interpreter)
                  (cons (replace-regexp-in-string
                         "^/bin/" "/data/data/com.termux/files/usr/bin/"
                         interpreter)
                        args))
                 ;; Return original for everything else
                 (t interpreter-info))))))

        (advice-add 'eshell-script-interpreter :around #'my/termux-eshell-script-interpreter)))

    ;; Bind C-l to recenter-top-bottom in normal/visual state, keep clear in insert state
    (evil-define-key 'normal eshell-mode-map (kbd "C-l") 'recenter-top-bottom)
    (evil-define-key 'visual eshell-mode-map (kbd "C-l") 'recenter-top-bottom)

    )

  ;;; Vterm
  (with-eval-after-load 'vterm
    ;; Bind C-l to recenter-top-bottom in normal/visual state, keep clear in insert state
    (evil-define-key 'normal vterm-mode-map (kbd "C-l") 'recenter-top-bottom)
    (evil-define-key 'visual vterm-mode-map (kbd "C-l") 'recenter-top-bottom))

  ;;; Slack
  ;; Slack configuration should look like the following
  ;;
  ;; $ pass edit slack-token
  ;;
  ;; And the content should look like the following:
  ;;
  ;; xoxd-...
  ;; username: username@domain
  ;; token: xoxc-...
  ;;
  ;; The first line xoxd should be URL encoded with % characters.
  (with-eval-after-load 'slack
    (slack-register-team
     :name "emacs-slack"
     :default t
     :client-id (auth-source-pass-get "username" "slack-token")
     :token (auth-source-pass-get "token" "slack-token")
     :cookie (auth-source-pass-get 'secret "slack-token")
     :subscribed-channels '(general slackbot))

    ;; Currently `slack-all-unread' does not work properly. This is a workaround.
    (defun slack-all-unreads ()
      "Select all unread rooms."
      (interactive)
      (slack-select-unread-rooms)))

  ;;; ASCII Image
  (defvar my/image-ascii-file-extensions '("jpg" "jpeg" "png" "gif" "bmp" "tiff")
    "List of image file extensions to be displayed as ASCII art.")

  (defun my/get-image-dimensions-imk (filepath)
    "Return the dimensions of the image at FILEPATH as a cons cell.

This function uses the 'magick identify' command to get the dimensions of the image."
    (let* ((output (with-output-to-string
                     (call-process "magick" nil standard-output nil
                                   "identify" "-format" "%w %h" filepath)))
           (dimensions (split-string output)))
      (cons (string-to-number (nth 0 dimensions))
            (string-to-number (nth 1 dimensions)))))

  (defun my/image-ascii-display ()
    "Display the current buffer's file as ASCII art using image2ascii with color support."
    (when buffer-file-name
      (let ((inhibit-read-only t)
            (ascii-image-ratio (/ (float (- (window-width) 1)) (car (my/get-image-dimensions-imk buffer-file-name)))))
        (erase-buffer)
        (let ((exit-code (call-process "image2ascii" nil t nil "-f" buffer-file-name
                                       "-r" (number-to-string ascii-image-ratio))))
          (if (zerop exit-code)
              (progn
                ;; Process ANSI color codes
                (ansi-color-apply-on-region (point-min) (point-max))
                (goto-char (point-min)))
            (message "Error processing image with image2ascii"))
          (set-buffer-modified-p nil)
          (read-only-mode 1)))))

  (define-derived-mode image-ascii-mode fundamental-mode "Image-ASCII"
    "Major mode for viewing images as ASCII art."
    (my/image-ascii-display))

  (if my/termux-p
      (dolist (ext my/image-ascii-file-extensions)
        (add-to-list 'auto-mode-alist (cons (concat "\\." ext "\\'") 'image-ascii-mode))))

  ;;; EAF
  (with-eval-after-load 'eaf
    (setopt eaf-apps-to-install '(browser))

    ;; EAF is not stable yet. This is a workaround to prevent crashes.
    (defun my/safe-eaf-call-sync (orig-fun &rest args)
      "Safely wrap around `eaf-call-sync' to prevent crashes."
      (if (and (string= (nth 0 args) "execute_function")
               (string= (nth 2 args) "is_focus"))
          ;; Instead of calling the actual function, return a default value
          nil ;; Assuming 'nil' is a safer return value for "is_focus"
        (apply orig-fun args)))

    (if my/crostini-p
        (advice-add 'eaf-call-sync :around #'my/safe-eaf-call-sync))

    ;; Disable modeline in eaf-mode
    (add-hook 'eaf-mode-hook (lambda () (setq mode-line-format nil)))
    )

  (with-eval-after-load 'go-mode
    (defun go-mode-setup ()
      (add-hook 'before-save-hook #'lsp-format-buffer t t)
      (add-hook 'before-save-hook #'lsp-organize-imports t t))

    ;; FIXME: Termux does not work with gopls somehow.
    (unless my/termux-p
      (add-hook 'go-mode-hook #'go-mode-setup)))

  ;;; Copilot
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))

  (with-eval-after-load 'copilot
    ;; The state nil works, but '(insert emacs hybrid) does not work. Why?
    (evil-define-key nil copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (evil-define-key nil copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
    (evil-define-key nil copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
    (evil-define-key nil copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
    (evil-define-key nil copilot-completion-map (kbd "C-<next>") 'copilot-next-completion)
    (evil-define-key nil copilot-completion-map (kbd "C-<prior>") 'copilot-previous-completion))

  (when (fboundp #'copilot-mode)
    (add-hook 'prog-mode-hook #'copilot-mode))

  ;;; ChatGPT
  (setq-default openai-key (auth-source-pass-get 'secret "platform.openai.com")
                openai-user (auth-source-pass-get "user" "platform.openai.com"))

  (let ((anthropic-api-key (auth-source-pass-get 'secret "api.anthropic.com"))
        (perplexity-api-key (auth-source-pass-get 'secret "api.perplexity.ai")))

    (with-eval-after-load 'gptel
      (setopt gptel-model 'gpt-4o-mini
              gptel-default-mode 'org-mode)

      (gptel-make-anthropic "Claude"
        :stream t
        :key anthropic-api-key)

      ;; Llama.cpp offers an OpenAI compatible API
      (gptel-make-openai "llama-cpp"          ;Any name
        :stream t                             ;Stream responses
        :protocol "http"
        :host "localhost:8080"                ;Llama.cpp server location
        :models '("Llama"))                   ;Any names, doesn't matter for Llama
      )

    (setopt chatgpt-shell-openai-key openai-key
            gptel-api-key openai-key)
    (setopt chatgpt-shell-anthropic-key anthropic-api-key)
    (setopt chatgpt-shell-perplexity-key perplexity-api-key)

    (setopt aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022" "--test-cmd" "pre-commit"))
    (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
    (spacemacs/set-leader-keys "$ a m" 'aider-transient-menu)
    )

  (defun my/visible-buffer-text ()
    "Return the visible text in the current buffer."
    (let ((text ""))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (if (not (invisible-p (point)))
              (setq text (concat text (char-to-string (following-char)))))
          (forward-char)))
      text))

  (defun my/chatgpt-shell-purpose-of-email (additional-prompt)
    "Ask ChatGPT for the purpose of an email."
    (interactive "sAdditional prompt: ")
    (chatgpt-shell-send-to-buffer
     (concat "Please tell me the purpose of this email very clearly and briefly to the point. "
             "Do not mention sender and receiver name. Say straightly that it's a talent acquisition, "
             "engineering team outsourcing, selling SaaS, or whatever."
             additional-prompt
             "\n\n"
             (my/visible-buffer-text))))

  (defun my/chatgpt-shell-reply-email (additional-prompt)
    "Ask ChatGPT to write a reply to an email with the given
`additional-prompt' in string. The content is written in the
current buffer."
    (interactive "sAdditional prompt: ")
    (let ((chatgpt-shell-prompt-query-response-style 'inline))
      (chatgpt-shell-send-to-buffer
       (concat "Please write a reply to the following email. "
               additional-prompt
               "\n\n"
               (buffer-substring-no-properties (point-min) (point-max))))))

  (defun my/chatgpt-shell-insert-natural-english (additional-prompt)
    "Ask ChatGPT to insert natural English."
    (interactive "sAdditional prompt: ")
    (let ((chatgpt-shell-prompt-query-response-style 'inline))
      (chatgpt-shell-send-to-buffer
       (concat "Could you make the following grammatically correct and natural? "
               additional-prompt))))

  (defun my/chatgpt-shell-dwim (additional-prompt)
    "Do What I Mean with ChatGPT. If the current buffer is a Gnus
Article mode, ask ChatGPT for the purpose of the email. If the
current buffer is a message-mode, ask ChatGPT to write a reply to
the email."
    (interactive "sAdditional prompt: ")
    (cond
     ((region-active-p)
      (cond
       ((string-empty-p additional-prompt) (chatgpt-shell-proofread-paragraph-or-region))
       ((string= "!" additional-prompt) (chatgpt-shell-quick-insert))
       ((string= "p" additional-prompt) (chatgpt-shell-prompt-compose nil))
       (t (chatgpt-shell-send-to-buffer
           (concat additional-prompt
                   "\n\n"
                   (buffer-substring-no-properties (region-beginning) (region-end)))))))
     ((or (eq major-mode 'gnus-article-mode)
          (eq major-mode 'notmuch-show-mode))
      (my/chatgpt-shell-purpose-of-email additional-prompt))
     ((or (eq major-mode 'message-mode)
          (eq major-mode 'notmuch-message-mode))
      (my/chatgpt-shell-reply-email additional-prompt))
     ((eq major-mode 'eshell-mode)
      ;; If eshell last command failed, summarize the output.
      (if (eq eshell-last-command-status 0)
          (chatgpt-shell-eshell-summarize-last-command-output)
        (chatgpt-shell-eshell-whats-wrong-with-last-command)))
     (t
      (my/chatgpt-shell-insert-natural-english additional-prompt))))

  (spacemacs/set-leader-keys "o a" 'my/chatgpt-shell-dwim)

  (autoload 'my/chatgpt-shell-dwim "chatgpt-shell")

  ;;; Claude Code
  (with-eval-after-load 'claude-code
    (spacemacs/set-leader-keys "$ C" 'claude-code-transient)
    (autoload 'claude-code-transient "claude-code" nil t)
    (setopt claude-code-terminal-backend 'vterm)

    ;; Custom function to display claude-code buffer on the right side
    (defun my/claude-code-display-buffer-right (buffer)
      "Display the claude code BUFFER on the right side of the current window."
      (display-buffer buffer '((display-buffer-in-direction)
                               (direction . right))))

    ;; Set claude-code to use the custom display function
    (setopt claude-code-display-window-fn #'my/claude-code-display-buffer-right)

    ;; Define your own hook listener function
    (defun my/claude-hook-listener (message)
      "Custom listener for Claude Code hooks.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
      (let ((hook-type (plist-get message :type))
            (buffer-name (plist-get message :buffer-name))
            (json-data (plist-get message :json-data))
            (args (plist-get message :args)))
        (cond
         ((eq hook-type 'notification)
          (message "Claude is ready in %s! JSON: %s" buffer-name json-data))
         ((eq hook-type 'stop)
          (message "Claude finished in %s! JSON: %s" buffer-name json-data))
         (t
          (message "Claude hook: %s with JSON: %s" hook-type json-data)))))

    ;; Add the hook listener using standard Emacs hook functions
    (add-hook 'claude-code-event-hook 'my/claude-hook-listener)

    )
  (require 'claude-code nil 'noerror)

  ;;; Convenient functions
  (defun my/kill-ring-save-unfilled (start end)
    (interactive "r")
    ;; Save the original major mode.
    (let ((original-mode major-mode)
          (text (buffer-substring-no-properties start end)))
      (with-temp-buffer
        ;; Set the major mode to the original one.
        (funcall original-mode)
        (insert text)
        (unfill-region (point-min) (point-max))
        (kill-ring-save (point-min) (point-max)))))

  (defun my/ediff-spacemacs-with-upstream ()
    (interactive)
    (ediff "~/.spacemacs" (file-truename "~/.spacemacs-upstream")))

  ;; Crostini specific configuration.
  (when my/crostini-p
    ;; Fix yank bug by comparing the kill-ring and the clipboard.
    (setopt select-enable-primary t)

    (defun my/evil-paste-fix-clipboard-advice (orig-fun &rest args)
      "Fix clipboard sync by comparing kill-ring with system clipboard."
      (let* ((clipboard (gui-get-selection))
             (select-enable-clipboard
              (and (stringp clipboard)
                   (not (string= (substring-no-properties clipboard)
                                 (substring-no-properties (or (car kill-ring) "")))))))
        (apply orig-fun args)))

    (advice-add 'evil-paste-after :around #'my/evil-paste-fix-clipboard-advice)
    (advice-add 'evil-paste-before :around #'my/evil-paste-fix-clipboard-advice))

  ;; Dirvish
  (use-package dirvish
    :defer t
    :config
    (defun my/dired-find-file-smart ()
      "Open directory in same window, file in other window."
      (interactive)
      (let ((file (dired-get-file-for-visit)))
        (if (file-directory-p file)
            (dired-find-file)
          (dired-find-file-other-window))))
    (evilified-state-evilify-map dirvish-mode-map
      :mode dired-mode
      :bindings
      "h" #'dired-up-directory
      "l" #'my/dired-find-file-smart
      "q" #'dirvish-quit
      "/" #'dirvish-narrow
      (kbd "<tab>") #'dirvish-subtree-toggle
      (kbd "TAB") #'dirvish-subtree-toggle
      "f" #'dirvish-layout-toggle
      "gf" #'dirvish-layout-toggle
      "gt" #'dirvish-layout-switch
      "gd" #'dirvish-dispatch
      "gl" #'dirvish-ls-switches-menu
      "gr" #'revert-buffer
      "g$" #'dired-hide-subdir
      "g?" #'dired-summary
      "gj" #'dired-next-dirline
      "gk" #'dired-prev-dirline
      "go" #'dired-view-file
      "gy" #'dired-show-file-type
      "gG" #'dired-do-chgrp
      "gO" #'dired-find-file-other-window
      (kbd "C-l") #'recenter-top-bottom
      )
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    (dirvish-override-dired-mode))
  (require 'dirvish nil 'noerror)

  ;; Frame keybindings
  (when (or my/crostini-p my/macos-p)
    (global-set-key (kbd "s-<up>") #'spacemacs/toggle-maximize-frame)
    (global-set-key (kbd "s-<down>") #'iconify-or-deiconify-frame))

  ;;; Services
  (with-eval-after-load 'prodigy
    (prodigy-define-service
      :name "Python Server (:8000)"
      :command "python"
      :args '("-m" "http.server" "8000")
      :cwd "~/services/python-server"
      :tags '(game)
      :stop-signal 'sigkill
      :kill-process-buffer-on-stop t)
    (prodigy-define-service
      :name "Cursor"
      :command "~/services/cursor-latest.AppImage"
      :cwd "~/services"
      :tags '(dev)
      :stop-signal 'sigkill
      :kill-process-buffer-on-stop t)
    (prodigy-define-service
      :name "LM Studio (:1234)"
      :command "~/services/LM-Studio-latest.AppImage"
      :cwd "~/services"
      :tags '(dev llm)
      :stop-signal 'sigkill
      :kill-process-buffer-on-stop t)
    (prodigy-define-service
      :name "llava (:1234)"
      :command "sh"
      :args '("./llava-latest.llamafile" "--port" "1234")
      :cwd "~/services"
      :tags '(dev llm)
      :stop-signal 'sigkill
      :kill-process-buffer-on-stop t)
    (prodigy-define-service
      :name "Llama (:1234)"
      :command "sh"
      :args '("./Llama-latest.llamafile" "--port" "1234")
      :cwd "~/services"
      :tags '(dev llm)
      :stop-signal 'sigkill
      :kill-process-buffer-on-stop t)
    )

  ;;; Git and Project
  (with-eval-after-load 'magit-status
    (setopt magit-display-buffer-function
            #'magit-display-buffer-same-window-except-diff-v1))

  (with-eval-after-load 'magit-diff
    ;; Workaround of C-<return> for text Terminal users
    (define-key magit-diff-section-map (kbd "g RET") #'magit-diff-visit-worktree-file))

  (with-eval-after-load 'projectile
    ;; By Spacemacs default, switch project asked me the project file to open.
    ;; But I prefer to open magit=status or the version-control (VC) buffer. It
    ;; gives me a chance to sync and handle VC related operations. I can still
    ;; do `projectile-find-file' after that if I really want to find a file.
    (setopt projectile-switch-project-action #'projectile-vc))

  (add-hook 'git-commit-mode-hook
            (lambda ()
              (yas-minor-mode 1)))

  ;;; Git Forge
  (with-eval-after-load 'forge
    ;; Set C-c C-o in forge-pullreq-mode to invoke code-review-forge-pr-at-point
    (evil-define-key nil forge-pullreq-mode-map (kbd "C-c C-o") #'code-review-forge-pr-at-point)

    (defun my/forge-insert-pullreqs-to-review ()
      "Insert a list of pull-requests to review."
      (let ((spec (forge--clone-buffer-topics-spec)))
        (oset spec reviewer github-username)
        (forge-insert-pullreqs spec "Pull requests to review")))

    (add-to-list 'magit-status-sections-hook #'my/forge-insert-pullreqs-to-review t))

  ;;; Code Review
  (with-eval-after-load 'code-review
    ;; RET key does not work for adding comments in code-review-mode. This
    ;; insert mode change can temporarily fix the issue for hybrid mode users.
    (evil-define-key 'motion code-review-mode-map (kbd "i") 'evil-insert)
    (evil-define-key '(insert emacs hybrid) code-review-mode-map (kbd "<escape>") 'evil-motion-state)

    (defun my/code-review-url-from-pullreq (&optional pr host)
      "Return the URL of the pull request PR on the HOST."
      (let* ((pr (or pr (code-review-db-get-pullreq) (code-review-pr-at-point)))
             (host (or host "www.github.com"))
             (owner (oref pr owner))
             (repo (oref pr repo))
             (number (oref pr number)))
        (format "https://%s/%s/%s/pull/%s" host owner repo number)))

    (defun my/code-review-browse-with-external-browser (&optional url)
      "Browse the current URL in the external browser."
      (interactive)
      (funcall browse-url-secondary-browser-function
               (or url (my/code-review-url-from-pullreq))))

    (spacemacs/set-leader-keys-for-major-mode 'code-review-mode
      "r" #'code-review-reload
      "vx" #'my/code-review-browse-with-external-browser
      )
    )

  ;; GitHub Settings
  (with-eval-after-load 'consult-gh
    (setopt
     consult-repo-action #'consult-gh--repo-browse-files-action
     consult-gh-issue-action #'consult-gh--issue-view-actioonsult-gh--code-view-action
     consult-gh-file-action #'consult-gh--files-view-action
     consult-gh-notifications-action #'consult-gh--notifications-action
     consult-gh-dashboard-action #'message
     consult-gh-large-file-warning-threshold 2500000
     consult-gh-prioritize-local-folder 'suggest
     ))

  (require 'tramp-gh)

  (defun my/projectile-project-root-ignore-remote (orig-fun &rest args)
    (unless (file-remote-p default-directory)
      (apply orig-fun args)))

  (advice-add 'projectile-project-root :around #'my/projectile-project-root-ignore-remote '((depth . -100)))

  ;;; More configuration follows
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
