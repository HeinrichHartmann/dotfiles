# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
#ZSH_THEME="intheclear"
#ZSH_THEME="random"
#ZSH_THEME="tjkirch"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=100

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git vagrant docker docker-compose)

source $ZSH/oh-my-zsh.sh

if [[ -n "$INSIDE_EMACS" ]]; then
    export EDITOR=emacsclient
    unset zle_bracketed_paste
fi

# http://zsh.sourceforge.net/Guide/zshguide02.html#l16
HISTSIZE=10000000
SAVEHIST=10000000

# use different history files for root and normal users
if (( ! EUID )); then
    HISTFILE=~/.zsh_history_root
else
    HISTFILE=~/.zsh_history
fi

# If this is set, zsh sessions will append their history list to the
# history file, rather than replace it. Thus, multiple parallel zsh
# sessions will all have the new entries from their history lists added
# to the history file, in the order that they exit. The file will still
# be periodically re-written to trim it when the number of lines grows
# 20% beyond the value specified by $SAVEHIST (see also the
# HIST_SAVE_BY_COPY option).
setopt APPEND_HISTORY

# This option both imports new commands from the history file, and also
# causes your typed commands to be appended to the history file (the
# latter is like specifying INC_APPEND_HISTORY, which should be turned
# off if this option is in effect). The history lines are also output
# with timestamps ala EXTENDED_HISTORY (which makes it easier to find
# the spot where we left off reading the file after it gets re-written).
#
# By default, history movement commands visit the imported lines as well
# as the local lines, but you can toggle this on and off with the
# set-local-history zle binding. It is also possible to create a zle
# widget that will make some commands ignore imported commands, and some
# include them.
#
# If you find that you want more control over when commands get
# imported, you may wish to turn SHARE_HISTORY off, INC_APPEND_HISTORY
# or INC_APPEND_HISTORY_TIME (see above) on, and then manually import
# commands whenever you need them using ‘fc -RI’.
setopt SHARE_HISTORY

# Save each command’s beginning timestamp (in seconds since the epoch)
# and the duration (in seconds) to the history file. The format of
# this prefixed data is:
setopt EXTENDED_HISTORY

# Remove superfluous blanks from each command line being added to the
# history list.
setopt HIST_REDUCE_BLANKS

# zsh history settings cf.
# http://unix.stackexchange.com/questions/273861/unlimited-history-in-zsh
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing nonexistent history.


# Insert data on ESC-.
function zsh-insert-date { LBUFFER+="$(date +%F)" }   # shell function
zle -N zsh-insert-date                                # create command
bindkey "^X." zsh-insert-date                         # Bind it to ESC-.

# PROMPT
PROMPT='%n@%m:%/ $(git_prompt_info)
$ '
PROMPT_EOL_MARK="<EOL>"

if [[ -z "$INSIDE_EMACS" ]]
then
  RPROMPT='$(date +"%Y-%m-%d %H:%M:%S") [$zsh_last_command_duration]'
fi

# show command timings in prompt
typeset -F SECONDS=0
function preexec() {
    zsh_command_timer=$SECONDS
}
function precmd() {
    if [ $zsh_command_timer ]; then
      zsh_last_command_duration=$(printf "%.3fs" $(($SECONDS - $zsh_command_timer)))
      unset zsh_command_timer
    fi
}

[[ -e ~/.allrc ]] &&  source ~/.allrc
