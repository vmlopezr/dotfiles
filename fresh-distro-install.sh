#!/bin/bash

# # Create folders
# mkdir ~/Documents/GitHub
# mkdir ~/Documents/development
# mkdir ~/Documents/utils

# # install zsh
# sudo dnf install zsh

# # install video codecs
# # sudo dnf install gstreamer1-plugins-{bad-\*,good-\*,base} gstreamer1-plugin-openh264 gstreamer1-libav --exclude=gstreamer1-plugins-bad-free-devel

# # install github desktop
# sudo rpm --import https://packagecloud.io/shiftkey/desktop/gpgkey
# sudo sh -c 'echo -e "[shiftkey]\nname=GitHub Desktop\nbaseurl=https://packagecloud.io/shiftkey/desktop/el/7/\$basearch\nenabled=1\ngpgcheck=0\nrepo_gpgcheck=1\ngpgkey=https://packagecloud.io/shiftkey/desktop/gpgkey" > /etc/yum.repos.d/shiftkey-desktop.repo'
# sudo dnf install github-desktop

# # install oh-my-zsh
# sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# # install syntax highlighting
# git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my=zsh/custom/plugins/zsh-syntax-highlighting

# TODO write script to install the contents
#      create simu-links of the dotfile dir contents
ln -s `pwd`/.doom.d ~/.doom.d
ln -s `pwd`/vmlopez.zsh-theme ~/.oh-my-zsh/custom/themes
ln -s `pwd`/.bashrc ~/.bashrc
ln -s `pwd`/.bash_aliases ~/.bash_aliases
ln -s `pwd`/.zshrc ~/.zshrc
ln -s `pwd`/.vimrc ~/.vimrc
ln -s `pwd`/.gitconfig ~/.gitconfig

# # TODO add option to remove ls background coloring
# # For fedora systems, hibernating causes issues with the backlight when reopening and attempting
# # to resume a session. Adding the following text has solved the issues.
# #echo "mem_sleep_default=deep" >> /etc/default/grub # add the text to the file starting with "GRUB_CMDLINE_LINUX="
# sudo grub2-mkconfig -o boot/efi/EFI/fedora/grub.cfg

# # The ethernet drivers do not work on install. The following command enables
# # ethernet.
# sudo sh -c 'modprobe -r e1000e; modprobe e1000e'
