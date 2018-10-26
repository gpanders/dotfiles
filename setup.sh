#!/bin/bash

shopt -s extglob

curr_dir=$(pwd)

if [ -s $HOME/.vim ]; then
  mv $HOME/.vim $HOME/.vim.bak
fi
ln -s $curr_dir/vim $HOME/.vim

if [ -s $HOME/.tmux ]; then
  mv $HOME/.tmux $HOME/.tmux.bak
fi
ln -s $curr_dir/tmux $HOME/.tmux

if [ -s $HOME/.vimrc ]; then
  mv $HOME/.vimrc $HOME/.vimrc.bak
fi
ln -s $curr_dir/vimrc $HOME/.vimrc

if [ -s $HOME/.tmux.conf ]; then
  mv $HOME/.tmux.conf $HOME/.tmux.conf.bak
fi
ln -s $curr_dir/tmux.conf $HOME/.tmux.conf

mkdir -p $HOME/.config/nvim
if [ -s $HOME/.config/nvim/init.vim ]; then
  mv $HOME/.config/nvim/init.vim $HOME/.config/nvim/init.vim.bak
fi
ln -s $curr_dir/nvim/init.vim $HOME/.config/nvim/init.vim

if [[ "$SHELL" =~ "bash" ]]; then
  if [ -s $HOME/.fzf.bash ]; then
    mv $HOME/.fzf.bash $HOME/.fzf.bash.bak
  fi
  ln -s $curr_dir/fzf.bash $HOME/.fzf.bash

  if [ -s $HOME/.bashrc ]; then
    mv $HOME/.bashrc $HOME/.bashrc.bak
  fi
  ln -s $curr_dir/bashrc $HOME/.bashrc
fi

# Upgrade vim-plug and install vim plugins
vim -u $curr_dir/vim/plugins.vim -c PlugUpgrade -c PlugInstall -c q -c q

# Initialize git submodules
git submodule update --init --remote

if hash tmux 2>/dev/null; then
  # Install tmux plugins in a background session
  tmux new-session -s install_plugins -d "tmux run-shell $HOME/.tmux/plugins/tpm/bindings/install_plugins"
fi

# Configure git
if ! git config --global --get user.name 1>/dev/null ; then
  git config --global user.name "Greg Anders"
fi

if ! git config --global --get user.email 1>/dev/null ; then
  read -r -p "Git email address: " git_email
  if [ ! -z $git_email ]; then
    git config --global user.email "$git_email"
  fi
fi

if hash zsh 2>/dev/null; then
  install_prezto=0
  read -r -p "Install prezto? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    install_prezto=1
  fi

  if [ $install_prezto -eq 1 ]; then
    if hash git 2>/dev/null; then
      if [ ! -d "${ZDOTDIR:-$HOME}/.zprezto" ]; then
        git clone --quiet --recursive https://github.com/gpanders/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
        cd "${ZDOTDIR:-$HOME}"/.zprezto && git submodule update --init --recursive && cd -
      else
        echo ".zprezto dir already exists, not cloning git repo"
      fi
      for zfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/!(README.md); do
        if [ ! -s "${ZDOTDIR:-$HOME}/.${zfile##*/}" ]; then
            ln -vs "$zfile" "${ZDOTDIR:-$HOME}/.${zfile##*/}"
        fi
      done
    else
      echo "Git not found; not cloning prezto"
    fi
  fi

  read -r -p "Change default shell to zsh? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    if ! [[ "$SHELL" =~ "zsh" ]]; then
      sudo chsh -s $(which zsh) ${SUDO_USER:-$USER}
    fi
  fi
fi

if [[ "$SHELL" =~ "bash" ]]; then
  read -r -p "Use .bash_aliases file? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    cp $curr_dir/bash_aliases $HOME/.bash_aliases
  fi
fi

if [ ! -d "$HOME/.pyenv" ]; then
  read -r -p "Install pyenv? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    ln -s $curr_dir/pyenvrc $HOME/.pyenvrc
    if [[ "$OSTYPE" == darwin* ]]; then
      brew install pyenv pyenv-virtualenv
    elif [[ "$OSTYPE" == linux-gnu ]]; then
      git clone https://github.com/pyenv/pyenv.git $HOME/.pyenv
      git clone https://github.com/pyenv/pyenv-virtualenv.git $HOME/.pyenv/plugins/pyenv-virtualenv
    fi
  fi
fi

echo "Install solarized dircolors?"
echo "    1) light"
echo "    2) dark"
echo "    3) universal"
read -r -p "Selection (default: none): " ans
solarized_dircolors=
case $ans in
  1) solarized_dircolors="light" ;;
  2) solarized_dircolors="dark" ;;
  3) solarized_dircolors="universal" ;;
esac
if [ ! -z "$solarized_dircolors" ]; then
  echo "Installing $solarized_dircolors dircolors to $HOME/.dir_colors"
  curl -fsLo $HOME/.dir_colors \
    https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-$solarized_dircolors
fi

if hash xmodmap 2>/dev/null; then
  read -r -p "Use .xmodmap file to set Caps Lock to Control? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    if [ -f "$HOME/.xmodmap" ]; then
      cat $curr_dir/xmodmap >> $HOME/.xmodmap
    else
      cp $curr_dir/xmodmap $HOME/.xmodmap
    fi
  fi
fi

echo " "
echo "Setup complete."
