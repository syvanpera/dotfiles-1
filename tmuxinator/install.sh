if test ! $(which tmuxinator)
then
  echo "  Installing tmuxinator for you."

  sudo gem install tmuxinator
fi

exit 0
