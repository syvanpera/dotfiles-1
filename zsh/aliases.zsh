alias reload!='. ~/.zshrc'
alias cls='clear' # Good 'ol Clear Screen command
alias l='ls -lah'
alias vi='nvim'
alias ag='ag --color-path 32 --color-line-number 34'
alias pubkey="more ~/.ssh/id_rsa.pub | pbcopy | echo '=> Public key copied to pasteboard.'"
alias vif='nvim $(fzf --height 40% --border)'
alias y='yarn'

# grc overides for ls
#   Made possible through contributions from generous benefactors like
#   `brew install coreutils`
if $(gls &>/dev/null)
then
  alias ls="gls -F --color"
  alias l="gls -lAh --color"
  alias ll="gls -l --color"
  alias la='gls -A --color'
fi
