# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export TERM="xterm-256color"
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh


# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="powerlevel10k/powerlevel10k"


DEFAULT_USER="scipio"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
 ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
 COMPLETION_WAITING_DOTS="true"

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
plugins=(git asdf battery emoji npm sudo command-not-found python pip github )

# User configuration

export PATH=$HOME/bin:/usr/local/bin:~/.local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

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

#emacs eterm 256
#export TERM="xterm-256color"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# utf-8
export LC_CTYPE=zh_TW.UTF-8

#asdf version manager setting
. $HOME/.asdf/asdf.sh

. $HOME/.asdf/completions/asdf.bash

# git aliases
alias ga='git add'
alias gp='git push'
alias gpu='git pull'

alias gs='git status'
alias gd='git diff'
alias gds='git diff --staged'

alias gm='git commit -m'
alias gc='git checkout'


#anaconda environment
export PATH="~/anaconda3/bin:$PATH"

# IEx setting
export ERL_AFLAGS="-kernel shell_history enabled"

# rename command in zsh
autoload zmv



source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/scipio/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/home/scipio/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/scipio/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/scipio/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

fpath=(/home/scipio/.asdf/completions /home/scipio/.oh-my-zsh/plugins/github /home/scipio/.oh-my-zsh/plugins/pip /home/scipio/.oh-my-zsh/plugins/python /home/scipio/.oh-my-zsh/plugins/command-not-found /home/scipio/.oh-my-zsh/plugins/sudo /home/scipio/.oh-my-zsh/plugins/npm /home/scipio/.oh-my-zsh/plugins/emoji /home/scipio/.oh-my-zsh/plugins/battery /home/scipio/.oh-my-zsh/plugins/asdf /home/scipio/.oh-my-zsh/plugins/git /home/scipio/.oh-my-zsh/functions /home/scipio/.oh-my-zsh/completions /usr/local/share/zsh/site-functions /usr/share/zsh/site-functions /usr/share/zsh/functions/Calendar /usr/share/zsh/functions/Chpwd /usr/share/zsh/functions/Completion /usr/share/zsh/functions/Completion/Base /usr/share/zsh/functions/Completion/Linux /usr/share/zsh/functions/Completion/Unix /usr/share/zsh/functions/Completion/X /usr/share/zsh/functions/Completion/Zsh /usr/share/zsh/functions/Exceptions /usr/share/zsh/functions/Math /usr/share/zsh/functions/MIME /usr/share/zsh/functions/Misc /usr/share/zsh/functions/Newuser /usr/share/zsh/functions/Prompts /usr/share/zsh/functions/TCP /usr/share/zsh/functions/VCS_Info /usr/share/zsh/functions/VCS_Info/Backends /usr/share/zsh/functions/Zftp /usr/share/zsh/functions/Zle)

autoload -Uz compinit && compinit

POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
