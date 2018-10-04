#!/bin/bash

shopt -s extglob

curr_dir=$(pwd)

mkdir -p $HOME/.vim
cp -vR $curr_dir/vim/* $HOME/.vim/
for pfile in $HOME/.vim/plugin/*.vim; do
  ln -f $pfile $curr_dir/vim/plugin/${pfile##*/}
done

if [ -f $HOME/.vimrc ]; then
  mv $HOME/.vimrc $HOME/.vimrc.bak
fi
mv -v $curr_dir/vimrc $HOME/.vimrc
ln $HOME/.vimrc $curr_dir/vimrc

if [ -f $HOME/.tmux.conf ]; then
  mv $HOME/.tmux.conf $HOME/.tmux.conf.bak
fi
mv -v $curr_dir/tmux.conf $HOME/.tmux.conf
ln $HOME/.tmux.conf $curr_dir/tmux.conf

curl -fsLo $HOME/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

if hash nvim 2>/dev/null; then
  echo "Found neovim installation, adding neovim specific configs"
  mkdir -p $HOME/.config/nvim
  cp -vR $curr_dir/nvim/* $HOME/.config/nvim/
  for pfile in $HOME/.config/nvim/config/*.vim; do
    ln -f $pfile $curr_dir/nvim/config/${pfile##*/}
  done

  # ln -vfs $HOME/.vimrc $HOME/.config/nvim/init.vim

  mkdir -p $HOME/.local/share/nvim/site/autoload
  cp -n $HOME/.vim/autoload/plug.vim $HOME/.local/share/nvim/site/autoload/plug.vim
  if [ ! -s $HOME/.local/share/nvim/site/plugin ]; then
    ln -vs $HOME/.vim/plugin $HOME/.local/share/nvim/site/plugin
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
      for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/!(README.md); do
        if [ ! -s "${ZDOTDIR:-$HOME}/.${rcfile##*/}" ]; then
            ln -vs "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile##*/}"
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

if [ ! -d "$HOME/.pyenv" ]; then
  install_pyenv=0
  read -r -p "Install pyenv? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    install_pyenv=1
  fi

  if [ $install_pyenv -eq 1 ]; then
    if [[ "$OSTYPE" == darwin* ]]; then
      brew install pyenv pyenv-virtualenv
    elif [[ "$OSTYPE" == linux-gnu ]]; then
      git clone https://github.com/pyenv/pyenv.git $HOME/.pyenv
      git clone https://github.com/pyenv/pyenv-virtualenv.git $HOME/.pyenv/plugins/pyenv-virtualenv
    fi
  fi
fi

read -r -p "Install custom fzf config? [y/N] " ans
if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
  cp $curr_dir/fzf.bash $HOME/.fzf.bash
fi

read -r -p "Install solarized dircolors? [y/N] " ans
if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
  curl -fsLo $HOME/.dir_colors \
    https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-universal
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
