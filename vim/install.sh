if [ ! -d ~/.config/nvim ]
then
  echo "  Installing NVim config."

  ln -s $(pwd)/vim/config ~/.config/nvim
fi

exit 0
