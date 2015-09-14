# Dotfiles

This is a modified version of Ben Alman's [dotfiles](https://github.com/cowboy/dotfiles). Here are the key differences:

- Stipped software installs to the bare minimum required for development to speed things up
- Emacs not VIM, this repo includes my own .emacs configuration too

## How the "dotfiles" command works

When [dotfiles][dotfiles] is run for the first time, it does a few things:

1. In Ubuntu, Git is installed if necessary via APT (it's already there in OSX).
1. This repo is cloned into your user directory, under `~/.dotfiles`.
1. Files in `/copy` are copied into `~/`. ([read more](#the-copy-step))
1. Files in `/link` are symlinked into `~/`. ([read more](#the-link-step))
1. You are prompted to choose scripts in `/init` to be executed. The installer attempts to only select relevant scripts, based on the detected OS and the script filename.
1. Your chosen init scripts are executed (in alphanumeric order, hence the funky names). ([read more](#the-init-step))

On subsequent runs, step 1 is skipped, step 2 just updates the already-existing repo, and step 5 remembers what you selected the last time. The other steps are the same.

## Installation

### OS X Notes

You need to have [XCode](https://developer.apple.com/downloads/index.action?=xcode) or, at the very minimum, the [XCode Command Line Tools](https://developer.apple.com/downloads/index.action?=command%20line%20tools), which are available as a much smaller download.

The easiest way to install the XCode Command Line Tools in OSX 10.9+ is to open up a terminal, type `xcode-select --install` and [follow the prompts](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/).

### Ubuntu Notes

Update APT with `sudo apt-get -qq update && sudo apt-get -qq dist-upgrade` first.

### Installation

1. Fork this repo
2. Open a terminal/shell and do this:

```sh
export github_user=YOUR_GITHUB_USER_NAME

bash -c "$(curl -fsSL https://raw.github.com/$github_user/dotfiles/master/bin/dotfiles)" && source ~/.bashrc
```

You might have to type your password in here and there.

## Inspiration
<https://github.com/cowboy/dotfiles>  
<https://github.com/gf3/dotfiles>  
<https://github.com/mathiasbynens/dotfiles>  

## License
Copyright (c) 2014 "Cowboy" Ben Alman
Licensed under the MIT license.
<http://benalman.com/about/license/>  
