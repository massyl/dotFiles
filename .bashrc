
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi


# some intelligent method to set the prompt
prompt_command () {
    RET=$?
    if [ $RET -eq 0 ]; then # set an error string for the prompt, if applicable
        ERRPROMPT=""
    else
        ERRPROMPT='($RET) '
    fi

    if [ "\$(type -t __git_ps1)" ]; then # if we're in a Git repo, show current branch
        BRANCH=" \$(__git_ps1 '(%s) ')"
    fi

    # setup preferences
    # set the titlebar to the last 2 fields of pwd
    local TITLEBAR='\[\e]2;`pwdtail`\a\]'

    # colors
    local GREEN="\[\033[0;32m\]"
    local DKGREEN="\[\033[1;32m\]"
    local CYAN="\[\033[0;36m\]"
    local DKCYAN="\[\033[1;36m\]"
    local BLUE="\[\033[0;34m\]"
    local DKBLUE="\[\033[1;34m\]"
    local GRAY="\[\033[0;37m\]"
    local DKGRAY="\[\033[1;30m\]"
    local WHITE="\[\033[1;37m\]"
    local RED="\[\033[0;31m\]"
    local DKRED="\[\033[1;31m\]"

    # return color to Terminal setting for text color
    local DEFAULT="\[\033[0;39m\]"
    # delete ${TITLEBAR} because it doesn't work inside the shell emacs or in the tty screen
    export PS1="${DKBLUE}\u${DKBLUE}@${DKBLUE} ${DKRED}$ERRPROMPT${DKBLUE} \w${DKGREEN}${BRANCH}${DEFAULT}$ "
}


#if [ "$color_prompt" = yes ]; then
 #   PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#else
 #   PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#fi

unset color_prompt force_color_prompt

# If tmux

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*|screen*)
   # PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
   prompt_command
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias glsdrm='git ls-files -d|xargs git rm'
alias grs='git remote show origin'
alias gpr='git pull --recruse-submodules'
alias gcr='git clone --recursive'
alias gfa='git fetch --all'
alias gco='git commit -m'
alias gst='git status'
alias gcn='git checkout -b '
alias gcb='git checkout '
alias glb='git branch '
alias wchd='watch -d ls -l'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'


# List all network connection (nm-applet = to have list of available network)
# xev : to get a keycode of keyboard key
# nmcli dev wifi con SSID password pwd name customName
# nmcli dev status
#alias nl = 'nmcli -p con list'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

alias lsd="ls -l | egrep  '^d'"
alias lsf="ls -l | egrep -v '^d'"

chmod 400  ~/.ssh/id_rsa
eval $(ssh-agent)
ssh-add ~/.ssh/id_rsa

export RUBYLIB=.:$RUBYLIB

alias vup="vagrant up"
alias vm1="vagrant ssh vm-1"
alias vm2="vagrant ssh vm-2"
alias vm3="vagrant ssh vm-3"
alias vm4="vagrant ssh vm-4"
alias emc='emacsclient'
alias emx='emacs -nw'
alias gfs='git flow feature start'
alias gff='git flow feature finish'

export _JAVA_AWT_WM_NONREPARENTING=1
# nitrogen --restore
#export PATH=$PATH:/opt/emacs24.5/bin

#maps Menu key same as windows key (serves as meta-key for xmonad, in order to have symetry with windows key)
xmodmap -e "keycode 135 = 0xffeb"
#disable capslock key
xmodmap -e "remove lock = 0x0000"
#maps capslock to enter
xmodmap -e "keycode 66 = 0xff0d"

#disable the right mouse button
#xmodmap -e 'pointer = 1 2 0 4 5 6 7 8 9'
#restaure the default mouse confirguration
#xmodmap -e 'pointer = default'
export SCALA_HOME=$HOME/softs/scala/current-scala
export SBT_HOME=$HOME/softs/scala/current-sbt
PATH=$PATH:$SCALA_HOME/bin:$SBT_HOME/bin
export PATH
# export PATH=/home/massyl/.stack/programs/x86_64-linux/ghc-7.10.3/bin:$PATH

# Test 2b machine of Kinvo
# alias test_2b="ssh -i .ssh/id_rsa admin@10.42.214.20"
# alias ha_mat="ssh -i .ssh/id_rsa admin@10.42.221.20"
# alias ha_test="ssh -i .ssh/id_rsa admin@10.42.242.20"

# Hack to fix xmonad crash when trying to share screen from hangout
xprop -root -f _NET_CLIENT_LIST_STACKING 32x -set _NET_CLIENT_LIST_STACKING 0


export GHC_HOME=$HOME/.stack/programs/x86_64-linux/ghc-8.0.2
export CABAL_HOME=$HOME/.local
export PATH=$HOME/bin:$GHC_HOME/bin:$CABAL_HOME/bin:$PATH

eval "$(stack --bash-completion-script stack)"

# Configure copy/yank as in emacs, to be used in terminal
copy_line_from_x_clipboard() {
    local n=$READLINE_POINT
    local l=$READLINE_LINE
    local s=$(xsel -o)
    READLINE_LINE=${l:0:$n}$s${l:$n:$((${#l}-n))}
    READLINE_POINT=$((n+${#s}))
}
bind -x '"\C-y": copy_line_from_x_clipboard'

alias e="SUDO_EDITOR=\"emacsclient -t -a emacs\" sudoedit"

export SAL_USE_VCLPLUGIN=gen lowriter

eval "$(pandoc --bash-completion)"

