export TERM="xterm-256color"
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh


# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="powerlevel9k/powerlevel9k"

# command line 左邊想顯示的內容
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir dir_writable vcs) # <= left prompt 設了 "dir"

# command line 右邊想顯示的內容
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status background_jobs history ram load time) # <= right prompt 設了 "time"

#nerd font
POWERLEVEL9K_MODE='nerdfont-complete'

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
plugins=(git asdf battery emacs emoji npm sudo command-not-found python pip github )

# User configuration

# export PATH=$HOME/bin:/usr/local/bin:$PATH
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
export TERM="xterm-256color"

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
export PATH="/Users/scipio/anaconda3/bin:$PATH"

# IEx setting
export ERL_AFLAGS="-kernel shell_history enabled"

# Rust setting
export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="/Users/scipio/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"


# rename command in zsh
autoload zmv

. $HOME/.asdf/asdf.sh

. $HOME/.asdf/completions/asdf.bash
source /usr/share/zsh-theme-powerlevel9k/powerlevel9k.zsh-theme
