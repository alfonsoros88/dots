# dots

    alias dots="/usr/bin/git --git-dir=$HOME/.dots.git/ --work-tree=$HOME"
    git clone --bare git@github.com:alfonsoros88/dots.git $HOME/.dots.git

# Ansible Setup

First install ansible

    $ sudo apt-add-repository ppa:ansible/ansible
    $ sudo apt-get update
    $ sudo apt-get install ansible
    $ cd setup
    $ ansible-playbook --ask-become-pass my_environment.yml
