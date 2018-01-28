# Sets reasonable macOS defaults.
#
# Or, in other words, set shit how I like in macOS.
#
# The original idea (and a couple settings) were grabbed from:
#   https://github.com/mathiasbynens/dotfiles/blob/master/.macos
#
# Run ./set-defaults.sh and you'll be good to go.

# Disable press-and-hold for keys in favor of key repeat.
defaults write -g ApplePressAndHoldEnabled -bool false

# Use AirDrop over every interface. srsly this should be a default.
defaults write com.apple.NetworkBrowser BrowseAllInterfaces 1

# Always open everything in Finder's list view. This is important.
defaults write com.apple.Finder FXPreferredViewStyle Nlsv

# Show the ~/Library folder.
chflags nohidden ~/Library

# Set fast key repeat.
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain InitialKeyRepeat -int 15

# Set the Finder prefs for showing a few different volumes on the Desktop.
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true

# Run the screensaver if we're in the bottom-left hot corner.
defaults write com.apple.dock wvous-bl-corner -int 5
defaults write com.apple.dock wvous-bl-modifier -int 0

# Hide Dock automatically
defaults write com.apple.dock autohide -bool true

# Hide Safari's bookmark bar.
defaults write com.apple.Safari ShowFavoritesBar -bool false

# Set up Safari for development.
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" -bool true
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Trackpad: enable tap to click for this user and for the login screen
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Increase sound quality for Bluetooth headphones/headsets
#defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40

# Use scroll gesture with the Ctrl (^) modifier key to zoom
defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true
defaults write com.apple.universalaccess HIDScrollZoomModifierMask -int 262144
# Follow the keyboard focus while zoomed in
defaults write com.apple.universalaccess closeViewZoomFollowsFocus -bool true


###############################################################################
# Finder                                                                      #
###############################################################################

# show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
# show path bar
defaults write com.apple.finder ShowPathbar -bool true
# allow text selection in Quick Look
defaults write com.apple.finder QLEnableTextSelection -bool true
# display full POSIX path as window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
# disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false


###############################################################################
# iTerm2                                                                      #
###############################################################################

# Specify the preferences directory
#defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "~/.iterm2"
# Tell iTerm2 to use the custom preferences in the directory
#defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true


###############################################################################
# Kill affected applications                                                  #
###############################################################################

for app in "Dock" "Finder" "Safari"; do
    killall "${app}" > /dev/null 2>&1
done
