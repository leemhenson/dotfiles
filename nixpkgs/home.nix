{ pkgs, ... }:

{
  home.file.".zshenv".text = ''
    export LANG=en_GB.UTF-8
    export LC_ALL=en_GB.UTF-8
  '';

  home.file.".direnvrc".text = ''
    use_ruby() {
      local ver=$1

      if [[ -z $ver ]] && [[ -f .ruby-version ]]; then
        ver=$(cat .ruby-version)
      fi

      if [[ ! -z $ver ]]; then
        chruby $ver
      fi
    }
  '';

  home.packages = [
    pkgs.bat
    pkgs.cheat
    pkgs.chruby
    pkgs.httpie
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
    '';
    oh-my-zsh = {
      custom = "$DOTFILES/oh-my-zsh";
      enable = true;
      plugins = [ "git" "httpie" "stack" "vi-mode" "z" ];
      theme = "custom";
    };
    sessionVariables = {
      CHEATCOLORS = true;
      CLICOLOR = true;
      DOTFILES = "$HOME/.dotfiles";
      DEFAULT_CHEAT_DIR = "$DOTFILES/cheatsheets";
    };
    shellAliases = {
      grom = "git rebase origin/master";
      gt = "git tree";
      j = "z";
      ls = "ls -alh";
    };
  };
}
