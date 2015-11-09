alias gs='git status '
alias gc='git commit '
alias ga='git add '
alias gb='git branch '
alias go='git checkout '
alias gd='git diff '
alias gk='gitk --all&'
alias gx='gitx --all'

# Update the GIT_HASH environment variable to the current
# commit's hash.
ghash() {
  export GHASH=$(git log -1 --pretty=format:%H)
}

# Perform a commit, but preserve hash.
# For use with git-rebase so that author time-stamps aren't
# messed up.
basec() {
  git commit -c $GHASH
}

# Reset head, but keep track of the original hash.
# Use instead of 'git reset HEAD^' when rebasing.
resh() {
  ghash && git reset HEAD^
}
