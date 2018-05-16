alias emacs=emacsclient

export PATH=~/.npm-global/bin:$PATH
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:/home/matt9j/go/bin

alias enable-agent="ssh-add /home/matt9j/.ssh/matt9j.pem; ssh-add /home/matt9j/.ssh/id_rsa; ssh-add /home/matt9j/.ssh/uw_keys.pem"

export TERM="xterm-256color"

export EDITOR="emacsclient"