########################################
## zplug conditions
########################################
source ~/.zplug/init.zsh
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "Jxck/dotfiles", use:"zsh/{http_status_codes,peco}.zsh"
zplug 'b4b4r07/zsh-history', as:command, use:misc/fzf-wrapper.zsh, rename-to:ff
zplug "b4b4r07/enhancd", use:enhancd.sh
zplug "djui/alias-tips"
# zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
zplug "k4rthik/git-cal", as:command
zplug "modules/prompt", from:prezto
# zplug "mafredri/zsh-async", defer:0
zplug "mollifier/anyframe", from:github
zplug "mollifier/cd-gitroot"
zplug "plugins/colorize", from:oh-my-zsh, defer:3
zplug "plugins/git",   from:oh-my-zsh
# zplug "plugins/osx", from:oh-my-zsh, if:"[[ $OSTYPE == *darwin* ]]"
zplug "plugins/compleat", from:oh-my-zsh
zplug "plugins/docker", from:oh-my-zsh
zplug "plugins/docker-compose", from:oh-my-zsh
zplug "plugins/gem", from:oh-my-zsh
zplug "plugins/gulp", from:oh-my-zsh
zplug "plugins/kubectl", from:oh-my-zsh
zplug "plugins/npm", from:oh-my-zsh
zplug "plugins/pip", from:oh-my-zsh
zplug "plugins/rails", from:oh-my-zsh
zplug "plugins/common-aliases", from:oh-my-zsh, defer:3
zplug "sindresorhus/pure", use:pure.zsh, as:theme
# zplug "tcnksm/docker-alias", use:zshrc
# zplug "themes/cloud", from:oh-my-zsh
# zplug "yous/lime", as:theme
# zplug "yous/vanilli.sh"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
# zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting", defer:2

zplug "lib/completion", from:oh-my-zsh
# zplug "dbz/zsh-kubernetes", from:github

# zplug check || zplug install
zplug load
########################################

## Prompt
autoload -Uz compinit promptinit
prompt pure

###
### Kubernetes prompt
###
# prompt_kubecontext() {
#   if [[ $(kubectl config current-context) == *"testing"* ]]; then
#         prompt_segment green black "(`kubectl config current-context`)"
#   elif [[ $(kubectl config current-context) == *"staging"* ]]; then
#         prompt_segment yellow black "(`kubectl config current-context`)"
#   elif [[ $(kubectl config current-context) == *"production"* ]]; then
#         prompt_segment red yellow "(`kubectl config current-context`)"
#   fi
# }

###
# Path to oh-my-zsh installation.
# export ZSH=$HOME/.oh-my-zsh

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
# plugins=(git docker git-extras tmux osx cdd github kubectl )

# source $ZSH/oh-my-zsh.sh

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

bindkey -e


# autoload -Uz compinit
# autoload -Uz compinit promptinit # 20170503
# if [ $(date +'%j') != $(stat -f '%Sm' -t '%j' ~/.zcompdump) ]; then
#     compinit
# else
#     compinit -C
# fi

# predict-on
# autoload predict-on

# ======================================================
# User configuration
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export PATH="$HOME/bin:$PATH"

# rbenv init
export RBENV_ROOT="$HOME/.rbenv/"
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
source ~/.zsh_aliases
# source ~/.dockerrc

#For golang
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin
export PATH="$PATH:/usr/local/opt/go/libexec/bin"

completion="$(brew --prefix)/share/zsh/site-functions/go"
if test -f $completion
then
    source <(cat $completion)
fi


#For perlbrew
# export PERLBREW_ROOT=${HOME}/.perl5/perlbrew
# source ${PERLBREW_ROOT}/etc/bashrc

# For RPaaS CLI
# _rpaas()
# {
#           local cmd
#           cmd=$(rpaas help | grep "^    \w" | awk -F' ' '{print $1;}' | sort | uniq)
#           _get_comp_words_by_ref cur
#            COMPREPLY=($(compgen -W "$cmd" -- "$cur"))
# }
# complete -F _rpaas rpaas

# For virtualenv
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    export WORKON_HOME=$HOME/.virtualenvs
    source /usr/local/bin/virtualenvwrapper_lazy.sh
fi

# For android
export ANDROID_HOME=/usr/local/opt/android-sdk

# For google-cloud-sdk
export PATH=$PATH:$HOME/google-cloud-sdk/bin
# # The next line enables bash completion for gcloud.
# source '/Users/yoshigoe/google-cloud-sdk/completion.bash.inc'

# colordiff
if [[ -x `which colordiff` ]]; then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

# For peco
pp() { peco | while read LINE; do $@ $LINE; done }


# pyenv
export PYENV_ROOT=/usr/local/var/pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# Homebrew
#export HOMEBREW_GITHUB_API_TOKEN='xx'

# Delay
export KEYTIMEOUT=0

# kubernetes
export PATH=$HOME/kubernetes/platforms/darwin/amd64:$PATH

# node
#export PATH=$PATH:~/.node/bin
export NODENV_ROOT=/usr/local/var/nodenv
if which nodenv > /dev/null; then eval "$(nodenv init -)"; fi

# zsh-completion
export fpath=(/usr/local/share/zsh-completions $fpath)

# genymotion_peco
export GENYMOTION_APP_HOME=$HOME/Applications/Genymotion.app
. $HOME/src/github.com/sys1yagi/genymotion-peco/bin/genymotion_peco.sh

# emacs shell
if [[ -n ${INSIDE_EMACS} ]]; then
    # This shell runs inside an Emacs *shell*/*term* buffer.
    prompt walters
    unsetopt zle
fi

export LC_ALL='en_US.UTF-8'

# The next line updates PATH for the Google Cloud SDK.
source '/Users/yoshigoe/google-cloud-sdk/path.zsh.inc'

# The next line enables shell command completion for gcloud.
source '/Users/yoshigoe/google-cloud-sdk/completion.zsh.inc'

# kubernetes
export KUBE_EDITOR=vim
source <(kubectl completion zsh)

# zsh performance
if (which zprof > /dev/null) ;then
  zprof | less
fi

# zcompile
# if [ ~/.zshrc -nt ~/.zshrc.zwc ]; then
#    zcompile ~/.zshrc
# fi
