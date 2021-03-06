{ pkgs, ... }:

let
  trunk = import <trunk> {};
in
{
  home.file = {
    ".config/bat/config" = {
      source = ~/.dotfiles/bat/config;
    };

    # errr no idea why this has suddenly stopped working?
    #   "Error installing file '.config/nvim/init.vim' outside $HOME"
    # seems ok if i just comment this out tho
    #
    # ".config/nvim/init.vim" = {
    #   source = ~/.dotfiles/nvim/init.vim;
    # };

    ".config/pgcli/config" = {
      source = ~/.dotfiles/pgcli/config;
    };

    ".config/vscode/wrapper/vscode" = {
      source = ~/.dotfiles/vscode/wrapper/vscode;
    };

    ".config/vscode/User" = {
      source = ~/.dotfiles/vscode/user-settings;
      recursive = true;
    };

    ".config/vscode-insiders/wrapper/code" = {
      source = ~/.dotfiles/vscode-insiders/wrapper/code;
    };

    ".config/vscode-insiders/User" = {
      source = ~/.dotfiles/vscode-insiders/user-settings;
      recursive = true;
    };

    ".direnvrc" = {
      text = ''
        use_ruby() {
          local ver=$1

          if [[ -z $ver ]] && [[ -f .ruby-version ]]; then
            ver=$(cat .ruby-version)
          fi

          if [[ ! -z $ver ]]; then
            chruby $version
          fi
        }
      '';
    };

    ".ghci" = {
      text = ''
        :set -package pretty-simple
        :set -interactive-print=Text.Pretty.Simple.pPrint
        :set +t
        :set prompt "\ESC[1;34m\STX%s\n\ESC[0;34m\STXλ> \ESC[m\STX"
      '';
    };

    ".taskrc" = {
      source = ~/.dotfiles/taskwarrior/taskrc;
    };

    ".zshenv" = {
      text = ''
        export LANG=en_GB.UTF-8
        export LC_ALL=en_GB.UTF-8
      '';
    };
  };

  home.homeDirectory = "/Users/leemhenson";

  home.packages = [
    pkgs.ammonite-repl
    pkgs.awscli2
    pkgs.bash
    pkgs.bat
    pkgs.cargo
    pkgs.cheat
    pkgs.chruby
    pkgs.coreutils
    pkgs.curl
    # pkgs.dhall-json
    pkgs.doctl
    pkgs.fd
    pkgs.gawk
    pkgs.git-crypt
    pkgs.gitAndTools.diff-so-fancy
    pkgs.gitAndTools.gh
    pkgs.gitAndTools.tig
    pkgs.gnupg
    pkgs.google-cloud-sdk
    pkgs.httpie
    pkgs.jq
    pkgs.mill
    pkgs.nodejs-slim-15_x
    pkgs.openssh
    pkgs.openssl
    pkgs.pgcli
    pkgs.readline
    pkgs.ripgrep
    pkgs.sbt
    pkgs.scala
    pkgs.stack
    pkgs.taskwarrior
    pkgs.tldr
    pkgs.tmux
    pkgs.wget
    pkgs.yarn
  ];

  nixpkgs.config = {
    allowUnsupportedSystem = true;
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.fzf = {
    defaultOptions = [ "-e" "--height 25%" "--reverse" ];
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.git = {
    aliases = {
      tree = "log --all --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date-order";
    };
    enable = true;
    extraConfig = ''
      [apply]
      whitespace = nowarn

      [color]
      diff = auto
      status = auto
      branch = auto
      ui = true
      interactive = auto

      [color "diff-highlight"]
      oldNormal = red bold
      oldHighlight = red bold 52
      newNormal = green bold
      newHighlight = green bold 22

      [core]
      editor = code --wait
      excludesfile = $XDG_CONFIG_HOME/git/ignore

      [diff]
      tool = Kaleidoscope

      [difftool]
      prompt = false

      [difftool "Kaleidoscope"]
      cmd = ksdiff --partial-changeset --relative-path \"$MERGED\" -- \"$LOCAL\" \"$REMOTE\"

      [fetch]
      prune = true

      [help]
      autocorrect = 1

      [merge]
      renameLimit = 1000000
      tool = vscode

      [mergetool]
      keepBackup = false

      [mergetool "Kaleidoscope"]
      cmd = ksdiff --merge --output \"$MERGED\" --base \"$BASE\" -- \"$LOCAL\" --snapshot \"$REMOTE\" --snapshot
      trustExitCode = true

      [mergetool "vscode"]
      cmd = code --wait $MERGED

      [pager]
      diff = diff-so-fancy | less --tabs=2 -RFX
      show = diff-so-fancy | less --tabs=4 -RFX

      [pull]
      rebase = true

      [push]
      default = current
    '';
    ignores = [
      "*~"
      ''\#*\#''
      ''\.#*''
      ".DS_Store"
      "/.dir-locals.el"
      "/.envrc"
      "/.vscode"
      "/npm-debug.log*"
      "/tags"
      "/tags.lock"
      "/tags.temp"
      "/vendor"
      "/yarn-error.log"
    ];
    signing = {
      key = "B1EA4611F4564B0C487DF4B44CC045383A6DCF55";
      signByDefault = true;
    };
    userEmail = "lee.m.henson@gmail.com";
    userName = "Lee Henson";
  };

  programs.home-manager = {
    enable = true;
    path = https://github.com/rycee/home-manager/archive/master.tar.gz;
  };

  programs.htop = {
    enable = true;
  };

  programs.man = {
    enable = true;
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "bitbucket.org" = {
        identityFile = "~/.ssh/bitbucket";
      };
      "github.com" = {
        identityFile = "~/.ssh/github";
      };
      "digitalocean.com" = {
        identityFile = "~/.ssh/smol";
      };
    };
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      path = "Documents/zsh/.histfile";
      save = 100000;
      size = 100000;
    };
    initExtra = ''
      KEYTIMEOUT=1

      # Do not require a leading '.' in a filename to be matched explicitly
      setopt GLOBDOTS

      # When searching for history entries in the line editor, do not display
      # duplicates of a line previously found, even if the duplicates are not contiguous.
      setopt HIST_FIND_NO_DUPS

      # If a new command line being added to the history list duplicates an older one,
      # the older command is removed from the list (even if it is not the previous event).
      setopt HIST_IGNORE_ALL_DUPS

      # Remove superfluous blanks from each command line being added to the history list
      setopt HIST_REDUCE_BLANKS

      # When writing out the history file, older commands that duplicate newer ones are omitted.
      setopt HIST_SAVE_NO_DUPS

      # immediately appends new commands to the histfile
      setopt INC_APPEND_HISTORY

      # Shaddapayourface
      unsetopt BEEP

      # clashes with INC_APPEND_HISTORY
      unsetopt SHARE_HISTORY

      source $HOME/.dotfiles/oh-my-zsh/plugins/vi-mode.zsh
      source $HOME/.nix-profile/etc/profile.d/nix.sh
      source $HOME/.nix-profile/share/chruby/chruby.sh

      case "$IN_NIX_SHELL" in
        impure) export PATH=''${PATH/$HOME\/.nix-profile\/bin:/} ;;
             *) export PATH="./node_modules/.bin:$HOME/.npm/global/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.local/bin:$HOME/.config/vscode/wrapper:$HOME/.config/vscode-insiders/wrapper:$PATH" ;;
      esac

      # This speeds up pasting w/ autosuggest
      # https://github.com/zsh-users/zsh-autosuggestions/issues/238

      pasteinit() {
        OLD_SELF_INSERT=''${''${(s.:.)widgets[self-insert]}[2,3]}
        zle -N self-insert url-quote-magic # I wonder if you'd need `.url-quote-magic`?
      }

      pastefinish() {
        zle -N self-insert $OLD_SELF_INSERT
      }

      zstyle :bracketed-paste-magic paste-init pasteinit
      zstyle :bracketed-paste-magic paste-finish pastefinish
    '';
    oh-my-zsh = {
      custom = "$DOTFILES/oh-my-zsh";
      enable = true;
      plugins = [ "docker" "git" "httpie" "taskwarrior" "vi-mode" "z" ];
      theme = "custom";
    };
    plugins = [
      {
        file = "alias-tips.plugin.zsh";
        name = "alias-tips";
        src = builtins.fetchGit {
          name = "djui-alias-tips";
          url = "https://github.com/djui/alias-tips.git";
          rev = "881ac952033dc62ba9ea3a215e45eb9b9b945362";
        };
      }
      {
        name = "fzf-z";
        src = builtins.fetchGit {
          name = "andrewferrier-fzf";
          url = "https://github.com/andrewferrier/fzf-z.git";
          rev = "4dee410557024671ae7763fce342009d03aa171f";
        };
      }
      {
        file = "nix.plugin.zsh";
        name = "nix-zsh-completions";
        src = builtins.fetchGit {
          name = "spwhitt-nix-zsh-completions";
          url = "https://github.com/spwhitt/nix-zsh-completions.git";
          rev = "8f1921160472307cbbc3e16813e57d3db7a2956e";
        };
      }
      {
        file = "nix-shell.plugin.zsh";
        name = "zsh-nix-shell";
        src = builtins.fetchGit {
          name = "chisui-zsh-nix-shell";
          url = "https://github.com/chisui/zsh-nix-shell.git";
          rev = "dceed031a54e4420e33f22a6b8e642f45cc829e2";
        };
      }
      {
        file = "zsh-syntax-highlighting.zsh";
        name = "zsh-syntax-highlighting";
        src = builtins.fetchGit {
          name = "zsh-users-zsh-syntax-highlighting";
          url = "https://github.com/zsh-users/zsh-syntax-highlighting.git";
          rev = "e900ad8bad53501689afcb050456400d7a8466e5";
        };
      }
    ];
    sessionVariables = {
      BAT_PAGER = "";
      CHEATCOLORS = "true";
      CLICOLOR = "true";
      DOTFILES = "$HOME/.dotfiles";
      DEFAULT_CHEAT_DIR = "$DOTFILES/cheatsheets";
      NPM_CONFIG_USERCONFIG = "$HOME/Documents/dotfiles/npmrc";
    };
    shellAliases = {
      awk = "gawk";
      grom = "git rebase origin/master";
      gt = "git tree";
      j = "z";
      ls = "ls -alh --color=auto";
      oni="/Applications/Onivim2.App/Contents/MacOS/Oni2";
    };
  };
}
