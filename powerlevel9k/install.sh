if [ ! -d ~/.powerlevel9k ]
then
  echo "  Installing Powerlevel9k prompt for you."

  git clone https://github.com/bhilburn/powerlevel9k.git ~/.powerlevel9k
else
  cd ~/.powerlevel9k && git pull
fi

exit 0
