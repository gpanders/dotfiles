#!/bin/bash

shopt -s extglob

curr_dir=$(pwd)

if [ -h $HOME/.vim ]; then
  rm $HOME/.vim
elif [ -d $HOME/.vim ]; then
  mv $HOME/.vim $HOME/.vim.bak
fi
ln -sv $curr_dir/vim $HOME/.vim

if [ -h $HOME/.tmux ]; then
  rm $HOME/.tmux
elif [ -d $HOME/.tmux ]; then
  mv $HOME/.tmux $HOME/.tmux.bak
fi
ln -sv $curr_dir/tmux $HOME/.tmux

if [ -h $HOME/.vimrc ]; then
  rm $HOME/.vimrc
elif [ -f $HOME/.vimrc ]; then
  mv $HOME/.vimrc $HOME/.vimrc.bak
fi
ln -sv $curr_dir/vimrc $HOME/.vimrc

if [ -h $HOME/.tmux.conf ]; then
  rm $HOME/.tmux.conf
elif [ -f $HOME/.tmux.conf ]; then
  mv $HOME/.tmux.conf $HOME/.tmux.conf.bak
fi
ln -sv $curr_dir/tmux.conf $HOME/.tmux.conf

mkdir -p $HOME/.config/nvim
if [ -h $HOME/.config/nvim/init.vim ]; then
  rm $HOME/.config/nvim/init.vim
elif [ -f $HOME/.config/nvim/init.vim ]; then
  mv $HOME/.config/nvim/init.vim $HOME/.config/nvim/init.vim.bak
fi
ln -sv $curr_dir/nvim/init.vim $HOME/.config/nvim/init.vim

if [[ "$SHELL" =~ "bash" ]]; then
  if [ -h $HOME/.fzf.bash ]; then
    rm $HOME/.fzf.bash
  elif [ -f $HOME/.fzf.bash ]; then
    mv $HOME/.fzf.bash $HOME/.fzf.bash.bak
  fi
  ln -sv $curr_dir/fzf.bash $HOME/.fzf.bash

  if [ -h $HOME/.bashrc ]; then
    rm $HOME/.bashrc
  elif [ -f $HOME/.bashrc ]; then
    mv $HOME/.bashrc $HOME/.bashrc.bak
  fi
  ln -sv $curr_dir/bashrc $HOME/.bashrc
fi

# Use .Xmodmap to set Caps Lock to Control
if hash xmodmap 2>/dev/null; then
  if [ -h "$HOME/.Xmodmap" ]; then
    rm $HOME/.Xmodmap
  elif [ -f "$HOME/.Xmodmap" ]; then
    mv $HOME/.Xmodmap $HOME/.Xmodmap.bak
  fi
  ln -s $curr_dir/Xmodmap $HOME/.Xmodmap
fi

# Upgrade vim-plug and install vim plugins
if [ ! -d $curr_dir/vim/plug ]; then
  vim -u $curr_dir/vim/plugins.vim -c PlugUpgrade -c PlugInstall -c qall!
fi

if hash tmux 2>/dev/null; then
  # Install tmux plugins in a background session
  git submodule update --init --remote tmux/plugins/tpm
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

# Pull suckless terminal and apply patches
git submodule update --init st
ln -sf $curr_dir/config.h $curr_dir/st/config.h
if ! hash st 2>/dev/null; then
  read -r -p "Install st? [y/N] " ans
  if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
    (cd st && curl -s https://st.suckless.org/patches/solarized/st-no_bold_colors-20170623-b331da5.diff | git apply && sudo make clean install)
  fi
fi

read -r -p "Install prezto? [y/N] " ans
if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
  if [ -h $HOME/.zprezto ]; then
    rm $HOME/.zprezto
  elif [ -d $HOME/.zprezto ]; then
    mv $HOME/.zprezto $HOME/.zprezto.bak
  fi

  git submodule update --init --recursive zprezto
  ln -s $curr_dir/zprezto $HOME/.zprezto

  for zfile in $curr_dir/zprezto/runcoms/!(README.md|zshenv); do
    if [ -h "$HOME/.${zfile##*/}" ]; then
      rm "$HOME/.${zfile##*/}"
    elif [ -f "$HOME/.${zfile##*/}" ]; then
      mv "$HOME/.${zfile##*/}" "$HOME/.${zfile##*/}.bak"
    fi
    ln -vs "$zfile" "$HOME/.${zfile##*/}"
  done

  # Use an untracked copy of zshenv to store sensitive node-specific config
  if [ -h $HOME/.zshenv ]; then
    rm $HOME/.zshenv
  elif [ -f $HOME/.zshenv ]; then
    mv $HOME/.zshenv $HOME/.zshenv.bak
  fi
  cp $curr_dir/zprezto/runcoms/zshenv $HOME/.zshenv

  # Install 3rd party (contrib) modules
  mkdir -p $curr_dir/zprezto/contrib
  if [ ! -d $curr_dir/zprezto/contrib/fzf ]; then
    git clone --quiet --recursive https://github.com/gpanders/fzf-prezto.git $curr_dir/zprezto/contrib/fzf
  fi
fi

if hash zsh 2>/dev/null; then
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

# Setup conky and i3
mkdir -p $HOME/.config/i3
if [ -h $HOME/.conkyrc ]; then
  rm $HOME/.conkyrc
elif [ -f $HOME/.conkyrc ]; then
  mv $HOME/.conkyrc $HOME/.conkyrc.bak
fi
ln -s $curr_dir/conkyrc $HOME/.conkyrc
ln -sf $curr_dir/i3/config $HOME/.config/i3/config
ln -sf $curr_dir/i3/conky-i3bar $HOME/.config/i3/conky-i3bar

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

echo " "
echo "Setup complete."
