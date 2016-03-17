# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/leemhenson/.fzf/bin* ]]; then
  export PATH="$PATH:/Users/leemhenson/.fzf/bin"
fi

# Man path
# --------
if [[ ! "$MANPATH" == */Users/leemhenson/.fzf/man* && -d "/Users/leemhenson/.fzf/man" ]]; then
  export MANPATH="$MANPATH:/Users/leemhenson/.fzf/man"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/Users/leemhenson/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/Users/leemhenson/.fzf/shell/key-bindings.zsh"

fkill() {
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    kill -${1:-9} $pid
  fi
}

fbr() {
  local branches branch
  branches=$(git branch -vv) &&
  branch=$(echo "$branches" | fzf +m) &&
  git checkout $(echo "$branch" | awk '{print $1}' | sed "s/.* //")
}

fcoc() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

unalias z 2> /dev/null
z() {
  if [[ -z "$*" ]]; then
    cd "$(_z -l 2>&1 | fzf +s --tac | sed 's/^[0-9,.]* *//')"
  else
    _z "$@"
  fi
}
