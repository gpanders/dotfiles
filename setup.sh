#!/bin/bash

shopt -s extglob

curr_dir=$(pwd)

function install() {
  if [[ "$OSTYPE" == darwin* ]]; then
    brew install "$@"
  elif [[ "$OSTYPE" == linux-gnu ]]; then
    if hash apt-get 2>/dev/null; then
      sudo apt-get install -y "$@"
    elif hash yum 2>/dev/null; then
      sudo yum install "$@"
    elif hash pacman 2>/dev/null; then
      sudo pacman -S "$@"
    fi
  fi
}

if ! hash stow 2>/dev/null; then
  install stow
fi

echo "Creating symlinks for vim"
stow -t $HOME vim
echo "Creating symlinks for neovim"
stow -t $HOME neovim
echo "Creating symlinks for emacs"
stow -t $HOME emacs
echo "Creating symlinks for tmux"
stow -t $HOME tmux
echo "Installing git template"
stow -t $HOME git

if [[ "$SHELL" =~ "bash" ]]; then
  echo "Installing symlinks for bash"
  stow -t $HOME bash
elif [[ "$SHELL" =~ "zsh" ]]; then
  echo "Installing symlinks for zsh"
  stow -t $HOME zsh
fi

if [ -d $curr_dir/private ]; then
  echo "Installing private symlinks"
  stow -t $HOME private
fi

if [[ "$OSTYPE" == linux-gnu ]] && ! hash i3 2>/dev/null; then
  read -r -p "Install i3? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    install i3wm conky
  fi
fi

if hash i3 2>/dev/null; then
  echo "Creating symlinks for i3"
  stow -t $HOME i3

  echo "Creating symlinks for conky"
  stow -t $HOME conky
fi

if ! hash offlineimap 2>/dev/null; then
  read -r -p "Install offlineimap? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    install offlineimap
  fi
fi

if hash offlineimap 2>/dev/null; then
  echo "Creating symlinks for offlineimap"
  stow -t $HOME offlineimap
fi

if ! hash mutt 2>/dev/null && ! hash neomutt 2>/dev/null; then
  read -r -p "Install mutt? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    install neomutt
  fi
fi

if hash neomutt 2>/dev/null; then
  if ! hash mutt 2>/dev/null; then
    ln -s $(which neomutt) /usr/local/bin/mutt
  else
    echo "Both mutt and neomutt installed!"
  fi
  echo "Creating symlinks for mutt"
  stow -t $HOME mutt
fi

# Create cache directories for vim
mkdir -p $HOME/.vim/cache/{backup,undo,swap}

if hash tmux 2>/dev/null; then
  # Install tmux plugins in a background session
  git submodule update --init
  tmux new-session -s install_plugins -d "tmux run-shell $HOME/.tmux/plugins/tpm/bindings/install_plugins"
fi

# Configure git
if ! git config --global --get user.name 1>/dev/null ; then
  git config --global user.name "Greg Anders"
fi

if ! git config --global --get user.email 1>/dev/null ; then
  read -r -p "Git email address [greg@gpanders.com]: " ans
  git config --global user.email "${ans:-greg@gpanders.com}"
fi

git config --global init.templatedir "$HOME/.config/git/template"
git config --global alias.a add
git config --global alias.b branch
git config --global alias.cm commit
git config --global alias.co checkout
git config --global alias.ctags "!.git/hooks/ctags"
git config --global alias.d diff
git config --global alias.f fetch
git config --global alias.l log
git config --global alias.m merge
git config --global alias.p push
git config --global alias.re reset
git config --global alias.st status

# if [ ! -d $HOME/.zprezto ]; then
#   read -r -p "Install prezto? [y/N] " ans
#   if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then

#     git clone --recursive https://github.com/gpanders/prezto.git $HOME/.zprezto

#     for rcfile in $HOME/.zprezto/runcoms/!(README.md|zshenv); do
#       if [ -h "$HOME/.${rcfile##*/}" ]; then
#         rm "$HOME/.${rcfile##*/}"
#       elif [ -f "$HOME/.${rcfile##*/}" ]; then
#         mv "$HOME/.${rcfile##*/}" "$HOME/.${rcfile##*/}.bak"
#       fi
#       ln -vs "$rcfile" "$HOME/.${rcfile##*/}"
#     done

#     # Use an untracked copy of zshenv to store sensitive node-specific config
#     if [ -h $HOME/.zshenv ]; then
#       rm $HOME/.zshenv
#     elif [ -f $HOME/.zshenv ]; then
#       mv $HOME/.zshenv $HOME/.zshenv.bak
#     fi
#     cp $HOME/.zprezto/runcoms/zshenv $HOME/.zshenv

#     # Install 3rd party (contrib) modules
#     git clone --quiet --recursive https://github.com/belak/prezto-contrib.git $HOME/.zprezto/contrib
#     if [ ! -d $HOME/.zprezto/contrib/fzf ]; then
#       git clone --quiet --recursive https://github.com/gpanders/fzf-prezto.git $HOME/.zprezto/contrib/fzf
#     fi
#   fi
# fi

if hash zsh 2>/dev/null && hash finger 2>/dev/null; then
  user_shell=$(finger $USER 2>/dev/null | grep Shell | awk '{print $4}')
  if [[ $user_shell != *"zsh"* ]]; then
    read -r -p "Change default shell to zsh? [y/N] " ans
    if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
      sudo chsh -s $(which zsh) ${SUDO_USER:-$USER}
    fi
  fi
fi

if [ ! -d "$HOME/.pyenv" ]; then
  read -r -p "Install pyenv? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    stow -t $HOME pyenv
    if [[ "$OSTYPE" == darwin* ]]; then
      brew install pyenv pyenv-virtualenv
    elif [[ "$OSTYPE" == linux-gnu ]]; then
      git clone https://github.com/pyenv/pyenv.git $HOME/.pyenv
      git clone https://github.com/pyenv/pyenv-virtualenv.git $HOME/.pyenv/plugins/pyenv-virtualenv
    fi
  fi
fi

# echo "Install solarized dircolors?"
# echo "    1) light"
# echo "    2) dark"
# echo "    3) universal"
# read -r -p "Selection (default: none): " ans
# solarized_dircolors=
# case $ans in
#   1) solarized_dircolors="light" ;;
#   2) solarized_dircolors="dark" ;;
#   3) solarized_dircolors="universal" ;;
# esac
# if [ ! -z "$solarized_dircolors" ]; then
#   echo "Installing $solarized_dircolors dircolors to $HOME/.dir_colors"
#   curl -fsLo $HOME/.dir_colors \
#     https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-$solarized_dircolors
# fi

echo " "
echo "Setup complete."
