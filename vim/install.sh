if [ ! -d ~/.config/nvim ]
then
  echo "  Installing NVim."

  ln -s $(pwd)/vim/config ~/.config/nvim
fi

pip2 install neovim --upgrade
pip3 install neovim --upgrade

exit 0
