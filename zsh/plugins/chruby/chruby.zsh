# because chruby is added to preexec_functions and direnv to precmd_functions,
# we end up with a PATH that will always find gems in the chruby installation
# even if we've set up direnv to find them locally in a project (e.g. ./vendor/bin).

# so here we move the chruby invocation from preexec_functions to precmd_functions
# before the direnv hook is added, which means things happen in the right order
# for direnv to have the effect we want.

if [[ "$preexec_functions" == *chruby_auto* ]]; then
  preexec_functions=(${preexec_functions#chruby_auto})
fi

if [[ ! "$precmd_functions" == *chruby_auto* ]]; then
  precmd_functions+=("chruby_auto")
fi

eval "$(direnv hook zsh)"
