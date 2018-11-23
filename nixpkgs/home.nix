{ pkgs, ... }:

{
  home.file = {
    ".config/pgcli/config" = {
      source = ~/.dotfiles/pgcli/config;
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

    ".zshenv" = {
      text = ''
        export LANG=en_GB.UTF-8
        export LC_ALL=en_GB.UTF-8
      '';
    };
  };

  home.packages = [
    pkgs.awscli
    pkgs.bat
    pkgs.cheat
    pkgs.chruby
    pkgs.curl
    pkgs.gitAndTools.diff-so-fancy
    pkgs.fd
    pkgs.gcc
    pkgs.httpie
    pkgs.jq
    # pkgs.neovim
    pkgs.nodejs-10_x
    pkgs.openssh
    pkgs.openssl
    pkgs.pgcli
    pkgs.readline
    pkgs.ripgrep
    pkgs.terraform
  ];

  programs.bash = {
    enable = true;
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
      tree = "log --all --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)%Creset' --abbrev-commit --date-order";
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
  };

  programs.ssh = {
    enable = true;
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      save = 100000;
      size = 100000;
    };
    initExtra = ''
      KEYTIMEOUT=1

      # Do not require a leading '.' in a filename to be matched explicitly
      setopt GLOBDOTS
      # Shaddapayourface
      unsetopt BEEP

      # Remove command lines from the history list when the first character on the
      # line is a space, or when one of the expanded aliases contains a leading space
      setopt HIST_IGNORE_SPACE
      # Remove superfluous blanks from each command line being added to the history list
      setopt HIST_REDUCE_BLANKS
      # immediately appends new commands to the histfile
      setopt INC_APPEND_HISTORY

      source $HOME/.dotfiles/oh-my-zsh/plugins/vi-mode.zsh
      source $HOME/.nix-profile/share/chruby/chruby.sh

      if [[ -d $PRIVATE_DOTFILES ]]; then
        for file in $PRIVATE_DOTFILES/zsh/*.zsh; do
          source "$file"
        done
      fi
    '';
    oh-my-zsh = {
      custom = "$DOTFILES/oh-my-zsh";
      enable = true;
      plugins = [ "docker" "git" "httpie" "stack" "vi-mode" "z" ];
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
      CHEATCOLORS = true;
      CLICOLOR = true;
      DOTFILES = "$HOME/.dotfiles";
      DEFAULT_CHEAT_DIR = "$DOTFILES/cheatsheets";
      PRIVATE_DOTFILES = "$HOME/.private-dotfiles";
    };
    shellAliases = {
      grom = "git rebase origin/master";
      gt = "git tree";
      j = "z";
      ls = "ls -alh";
    };
  };
}
