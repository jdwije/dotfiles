# Ubuntu-only stuff. Abort if not Ubuntu.
is_ubuntu || return 1

# Update APT.
e_header "Updating APT"
sudo apt-get -qq update
sudo apt-get -qq dist-upgrade

# Install APT packages.
packages=(
  aspell
  build-essential
  git-core
  libssl-dev
  nmap
  sl
  telnet
  tree
  emacs25
  xclip
  htop
  multitail
)

packages=($(setdiff "${packages[*]}" "$(dpkg --get-selections | grep -v deinstall | awk '{print $1}')"))

if (( ${#packages[@]} > 0 )); then
  e_header "Installing APT packages: ${packages[*]}"
  for package in "${packages[@]}"; do
    sudo apt-get -qq install "$package"
  done
fi

# Install Git Extras
if [[ ! "$(type -P git-extras)" ]]; then
  e_header "Installing Git Extras"
  (
    cd $DOTFILES/vendor/git-extras &&
    sudo make install
  )
fi

# Swap Left ALT with Left SUPER [aka. Windows Key]
xmodmap -e "keycode 64 = Super_L NoSymbol Super_L"    #this will make Alt_L to act as Super_L
xmodmap -e "keycode 133 = Alt_L Meta_L Alt_L Meta_L"  #this will make Super_L to act as Alt_L

xmodmap -pke > ~/.Xmodmap

echo 'xmodmap .Xmodmap' > ~/.xinitrc
