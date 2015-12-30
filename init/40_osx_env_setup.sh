# OSX-only stuff. Abort if not OSX.
is_osx || return 1

# disable .DS_Store file writing on network shares
defaults write com.apple.desktopservices DSDontWriteNetworkStores true
