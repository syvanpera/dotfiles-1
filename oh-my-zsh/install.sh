if [ ! -d ~/.oh-my-zsh ]; then
  echo "  Installing Oh-My-Zsh for you."

  umask g-w,o-w

  echo "Cloning Oh My Zsh...\n"
  env git clone --depth=1 https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh || {
    printf "Error: git clone of oh-my-zsh repo failed\n"
    exit 1
  }

  echo "Cloning Powerlevel9k theme...\n"
  env git clone --depth=1 https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k || {
    printf "Error: git clone of powerlevel9k repo failed\n"
    exit 1
  }
fi

exit 0
