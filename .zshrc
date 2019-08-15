# ======================================================
# Zsh configuration
unsetopt IGNORE_EOF
setopt PUSHD_IGNORE_DUPS
setopt AUTO_MENU
setopt AUTO_CD
setopt AUTO_PARAM_KEYS
# Zsh history
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt extended_history
setopt append_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history

# emacs keybind
bindkey -e

## Prompt
### Require npm install --global pure-prompt
autoload -Uz promptinit; promptinit
#prompt spaceship

## install font
## sudo apt-get install fonts-powerline # linux

# predict-on
# autoload predict-on

# Delay
export KEYTIMEOUT=0

# Lang
export LANG=en_US.UTF-8
export LC_ALL='en_US.UTF-8'

# ======================================================

########################################
## zplug conditions
########################################
export ZPLUG_LOADFILE=""
source ~/.zplug/init.zsh
# zplug clear
zplug 'zplug/zplug' #, hook-build:'zplug --self-manage'
zplug 'peco/peco', as:command, from:gh-r
zplug "mafredri/zsh-async", from:"github", use:"async.zsh"
# zplug "sindresorhus/pure", use:"pure.zsh", from:github, as:theme
zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme
zplug "Jxck/dotfiles", use:"zsh/{http_status_codes,peco}.zsh"
zplug 'b4b4r07/zsh-history', as:command, use:misc/fzf-wrapper.zsh, rename-to:ff
# zplug "b4b4r07/enhancd", use:init.sh
zplug "djui/alias-tips"
# zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
# zplug "modules/prompt", from:prezto
zplug "mollifier/anyframe"
zplug "mollifier/cd-gitroot", lazy:true
zplug "plugins/colorize", from:oh-my-zsh, defer:3
zplug "plugins/git", from:oh-my-zsh
# zplug "plugins/osx", from:oh-my-zsh, if:"[[ $OSTYPE == *darwin* ]]"
# zplug "plugins/compleat", from:oh-my-zsh
zplug "plugins/docker", from:oh-my-zsh, defer:3
zplug "plugins/docker-compose", from:oh-my-zsh, defer:3
zplug "plugins/aws", from:oh-my-zsh
# zplug "plugins/gem", from:oh-my-zsh
# zplug "plugins/gulp", from:oh-my-zsh
zplug "plugins/kubectl", from:oh-my-zsh, defer:3
zplug "plugins/github", from:oh-my-zsh
zplug "plugins/terraform", from:oh-my-zsh, defer:3
# zplug "plugins/npm", from:oh-my-zsh
# zplug "plugins/pip", from:oh-my-zsh
# zplug "plugins/rails", from:oh-my-zsh
zplug "plugins/common-aliases", from:oh-my-zsh, defer:3
# zplug "tcnksm/docker-alias", use:zshrc
# zplug "themes/cloud", from:oh-my-zsh
# zplug "yous/lime", as:theme
# zplug "yous/vanilli.sh"zp
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
# zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting", defer:3
zplug "direnv/direnv", from:gh-r, as:command, lazy:true
zplug "lib/completion", from:oh-my-zsh, defer:3
# zplug "knu/z", use:z.sh, defer:3
zplug "superbrothers/zsh-kubectl-prompt", from:github, use:"kubectl.zsh"

# zplug "dbz/zsh-kubernetes", from:github

# zplug check || zplug install
zplug load

########################################

## Anyframe
autoload -Uz anyframe-init
anyframe-init
zstyle ":anyframe:selector:" use peco

bindkey '^xb' anyframe-widget-cdr
bindkey '^x^b' anyframe-widget-checkout-git-branch

bindkey '^xr' anyframe-widget-execute-history
bindkey '^x^r' anyframe-widget-execute-history

bindkey '^xi' anyframe-widget-put-history
bindkey '^x^i' anyframe-widget-put-history

bindkey '^xg' anyframe-widget-cd-ghq-repository
bindkey '^x^g' anyframe-widget-cd-ghq-repository

bindkey '^xk' anyframe-widget-kill
bindkey '^x^k' anyframe-widget-kill

bindkey '^xe' anyframe-widget-insert-git-branch
bindkey '^x^e' anyframe-widget-insert-git-branch

## fzf (not used)
export FZF_TMUX=1
export FZF_TMUX_HEIGHT=10
export FZF_DEFAULT_OPTS="--cycle --select-1 --ansi --multi"

# source $ZSH/oh-my-zsh.sh

# User configuration
# export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
export PATH="$HOME/bin:$PATH"

# enhancd
export ENHANCD_FILTER=peco:fzf

##########################
# Aliases of mine
##########################
# alias zshconfig="mate ~/.zshrc"
source ~/.zsh_aliases

# For virtualenv
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    export WORKON_HOME=$HOME/.virtualenvs
    source /usr/local/bin/virtualenvwrapper_lazy.sh
fi

# For android dev
export ANDROID_HOME=/usr/local/opt/android-sdk

# For google-cloud-sdk
export PATH=$PATH:$HOME/.google-cloud-sdk/bin
source $HOME/.google-cloud-sdk/path.zsh.inc
source $HOME/.google-cloud-sdk/completion.zsh.inc

# For peco
pp() { peco | while read LINE; do $@ $LINE; done }

# For golang
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin:/usr/local/opt/go/libexec/bin

### golang / brew
completion="$(brew --prefix)/share/zsh/site-functions/go"
if test -f $completion; then
    source <(cat $completion)
fi

# rbenv init
export RBENV_ROOT="$HOME/.rbenv/"
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init - --no-rehash)"

# pyenv
export PYENV_ROOT=/usr/local/var/pyenv
if which pyenv > /dev/null; then eval "$(pyenv init - --no-rehash)"; fi
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# nodenv
#export PATH=$PATH:~/.node/bin
export NODENV_ROOT=/usr/local/var/nodenv
if which nodenv > /dev/null; then
    eval "$(nodenv init - --no-rehash)";
fi
#
# direnv
export EDITOR="emacsclient -nw"
if type direnv >/dev/null 2>&1; then
    eval "$(direnv hook zsh)"
fi

# kubernetes
export KUBE_EDITOR="emacsclient -nw"
#source <(kubectl completion zsh)
# source /usr/local/etc/zsh-kubectl-prompt/kubectl.zsh
# export RPROMPT="%{$fg[blue]%}($ZSH_KUBECTL_PROMPT)%{$reset_color%}"

# For emacs shell
# if [[ -n ${INSIDE_EMACS} ]]; then
#     # This shell runs inside an Emacs *shell*/*term* buffer.
#     prompt walters
#     unsetopt zle
# fi

# zsh performance profile
if (which zprof > /dev/null) ;then
  zprof | less
fi

# zcompile
if [ ~/.zshrc -nt ~/.zshrc.zwc ]; then
  zcompile ~/.zshrc
fi

# mysql-client
export PATH="$PATH:/usr/local/opt/mysql-client/bin"


# iTerm2
# test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"


########################################
## private envs
########################################
source ~/.zsh_private


# autojump
[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh
