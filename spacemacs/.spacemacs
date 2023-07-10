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
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t
            c-c++-enable-clang-format-on-save t)
     ;; docker
     emacs-lisp
     eww
     git
     gnus
     (go :variables
         go-backend 'lsp
         go-use-golangci-lint t
         go-format-before-save t
         gofmt-command "goimports")
     ;; (groovy :variables
     ;;         groovy-indent-offset 2)
     (helm :variables
           ;; Prevent helm from taking over the entire frame.
           helm-show-completion-display-function
           #'helm-show-completion-default-display-function)
     ;; ivy
     (javascript :variables
                 js2-strict-missing-semi-warning nil)
     lsp
     markdown
     multiple-cursors
     (org :variables
          org-agenda-files (directory-files-recursively
                            (file-truename "~/Documents/projects") "\\.org$")
          org-enable-roam-support t
          org-enable-roam-ui t
          org-roam-directory (file-truename "~/Documents/roam/"))
     pass
     (python :variables
             python-formatter 'yapf
             python-format-on-save t)
     react
     reddit
     ;; renpy
     ;; ruby
     ;; rust
     search-engine
     (shell :variables
            shell-default-full-span nil
            shell-default-shell 'eshell)
     slack
     ;; spell-checking
     syntax-checking
     ;; terraform
     treemacs
     (typescript :variables
                 typescript-fmt-on-save t
                 typescript-fmt-tool 'typescript-formatter)
     version-control
     w3m
     yaml
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
     (copilot :location (recipe
                         :fetcher github
                         :repo "zerolfx/copilot.el"
                         :files ("*.el" "dist")))
     ob-chatgpt-shell
     ob-mermaid
     org-tree-slide
     ox-clip
     protobuf-mode
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

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
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

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
   ;; This has no effect in terminal or if "all-the-icons" package or the font
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
   ;; with 2 themes variants, one dark and one light)
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

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

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
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

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
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

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
   dotspacemacs-line-numbers 'relative

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
   dotspacemacs-enable-server nil

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
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

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

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

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
  (customize-set-variable
   'custom-file (file-truename (concat dotspacemacs-directory ".spacemacs-custom.el")) "Separate custom file")
  (load custom-file)
  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;;; Get user full name and mail from git config
  (setq-default user-full-name (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.name")))
  (setq-default user-mail-address (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.email")))

  ;;; Enable auth source pass
  (with-eval-after-load 'auth-source-pass
    (auth-source-pass-enable)
    (setq auth-sources '(password-store))
    (defun my/auth-source-pass-entries (host)
      (seq-filter (lambda (entry-data) (equal `("host" . ,host) (assoc "host" entry-data)))
                  (mapcar 'auth-source-pass-parse-entry (auth-source-pass-entries)))))

  (require 'auth-source-pass 'noerror)

  ;;; Set up buildifier for bazel
  (with-eval-after-load 'bazel
    (add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-buildifier nil t))))

  ;;; Reddit
  (with-eval-after-load 'reddigg
    (defadvice reddigg-view-comments (around no-query-string activate)
      "Remove query string from the url for `reddigg-view-comments'."
      (let ((cmt (car (split-string cmt "\\?"))))
        ad-do-it))

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

  (with-eval-after-load 'browse-url
    (defadvice browse-url-can-use-xdg-open (around termux-can-use-xdg-open activate)
      "Use termux-open if available."
      (if (executable-find "termux-open")
          (setq ad-return-value t)
        ad-do-it)))

  (with-eval-after-load 'eww
    ;; To open external browser from eww, press `, v x'
    (setq browse-url-browser-function 'eww
          browse-url-secondary-browser-function 'browse-url-default-browser)

    ;; Use browse-url-handlers for all URLs in eww. But the redirection URLs are
    ;; not working.
    (setq eww-use-browse-url "")

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
    (setq-default shr-use-fonts nil))

  ;;; `ob-mermaid'
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((mermaid . t)))
    (require 'ob-chatgpt-shell nil 'noerror))

  (with-eval-after-load 'ob-chatgpt-shell
    (ob-chatgpt-shell-setup))

  (with-eval-after-load 'ob-mermaid
    ;; Find the executable mmdc then fall back to local node_modules.
    (setq ob-mermaid-cli-path
          (or (executable-find "mmdc")
              (expand-file-name "~/node_modules/.bin/mmdc"))))

  ;;; Gnus

  ;; Get email, and store in nnml
  (setq gnus-secondary-select-methods
        (mapcar (lambda (entry-data)
                  (let ((host (cdr (assoc "host" entry-data)))
                        (user (cdr (assoc "user" entry-data))))
                    `(nnimap
                      ,(concat host "/" user)
                      (nnimap-user ,user)
                      (nnimap-address ,host)
                      (nnimap-server-port 993)
                      (nnimap-stream ssl))))
                (my/auth-source-pass-entries "imap.gmail.com")))

  ;; Send email via Gmail:
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com")

  ;; Archive outgoing email in Sent folder on imap.gmail.com:
  (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail")

  ;; Set return email address based on incoming email address
  (setq gnus-posting-styles
        (mapcar
         (lambda (user)
           `((header "to" ,user)
             (address ,user)))
         (mapcar
          (lambda (entry)
            (cdr (assoc "user" entry)))
          (my/auth-source-pass-entries "imap.gmail.com"))))

  ;; Store email in ~/gmail directory
  (setq nnml-directory "~/gmail")
  (setq message-directory "~/gmail")

  ;; Set up Gmail smtp
  (setq smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (with-eval-after-load 'gnus
    ;; Use y key to archive email
    (define-key gnus-summary-mode-map (kbd "y") 'gnus-summary-delete-article))

  ;;; Slack
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

  ;;; Copilot
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))

  (with-eval-after-load 'copilot
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
    (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
    (define-key copilot-completion-map (kbd "C-<next>") 'copilot-next-completion)
    (define-key copilot-completion-map (kbd "C-<prior>") 'copilot-previous-completion))

  (add-hook 'prog-mode-hook 'copilot-mode)

  ;;; ChatGPT
  (setq-default chatgpt-shell-openai-key (auth-source-pass-get 'secret "openai-key"))

  (defun my/chatgpt-shell-purpose-of-email (additional-prompt)
    "Ask ChatGPT for the purpose of an email."
    (interactive "sAdditional prompt: ")
    (chatgpt-shell-send-to-buffer
     (concat "Please help me understand the email sender's very rief purpose. "
             additional-prompt
             "\n\n"
             (buffer-substring-no-properties (point-min) (point-max)))))

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
     ((eq major-mode 'gnus-article-mode)
      (my/chatgpt-shell-purpose-of-email addional-prompt))
     ((eq major-mode 'message-mode)
      (my/chatgpt-shell-reply-email additional-prompt))
     ((region-active-p)
      (chatgpt-shell-send-and-review-region))
     (t
      (my/chatgpt-shell-insert-natural-english additional-prompt))))

  (spacemacs/set-leader-keys "o a" 'my/chatgpt-shell-dwim)

  ;;; Convenient functions
  (defun kill-ring-save-unfilled (start end)
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

  (defun ediff-spacemacs-with-upstream ()
    (interactive)
    (ediff "~/.spacemacs" (file-truename "~/.spacemacs-upstream")))

  ;;; More configuration follows
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
