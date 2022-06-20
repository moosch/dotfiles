. "$HOME/.cargo/env"

################
# Lua
################
export PATH="$PATH:$HOME/bin/lua-5.4.3/install/bin"

###############
# PSQL
# #############
export PATH="$PATH:/usr/local/Cellar/libpq/14.2/bin"
# ctags=/usr/local/bin/ctags


##############
## Pony Lang
##############
export PATH=/Users/moosch/.local/share/ponyup/bin:$PATH

###################
# Lisp Lem editor
###################
export PATH="$PATH:$HOME/.roswell/bin"


##################
# p7zip + more
##################
export PATH="$PATH:$HOME/bin"

################
# Ruby
###############
export PATH="/usr/local/opt/ruby/bin:$PATH"

export LDFLAGS="-L/usr/local/opt/openssl/lib"
export CFLAGS="-I/usr/local/opt/openssl/include"


export PATH="$PATH:$HOME/Applications/Racket v8.1/bin"


export PATH="$PATH:$HOME/development/hugo/bin"


###################
# ZIG
###################
export PATH="$PATH:$HOME/development/zig-0.9.0"



#####################
# AWS Profile
#####################
alias awsprofile='export AWS_PROFILE=$(cat ~/.aws/credentials | awk "/^\[/" | tr -d "[" | tr -d  "]" | fzf)'


################
# Ruby
###############
export PATH="/usr/local/opt/ruby/bin:$PATH"

export LDFLAGS="-L/usr/local/opt/openssl/lib"
export CFLAGS="-I/usr/local/opt/openssl/include"


export PATH="$PATH:$HOME/Applications/Racket v8.1/bin"


export PATH="$PATH:$HOME/development/hugo/bin"




# ##############################
# Annoying Flutter/Android stuff
# ##############################

# Android SDK tools
# export PATH="$PATH:`pwd`/Android/tools/bin"
export ANDROID_SDK_ROOT="$PATH:`pwd`/Library/Android"
export ANDROID_HOME="$PATH:`pwd`/Library/Android"
# New sdkmanager
# export PATH="$PATH:`pwd`/Library/Android/sdk/cmdline-tools/latest/bin"
# Emulator
# export PATH="$PATH:`pwd`/Library/Android/sdk/emulator"
# adb
# export PATH="$PATH:`pwd`/Library/Android/sdk/platform-tools"

export ANDROID_HOME=/Users/$USER/Library/Android/sdk
export ANDROID_SDK_ROOT=$ANDROID_HOME

export PATH="$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"
# PATH="$PATH:$ANDROID_HOME/emulator:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"
export PATH="$PATH:$HOME/.pub-cache/bin"

# Flutter
export PATH="$PATH:$HOME/development/flutter/bin"


export PATH="/usr/local/opt/icu4c/bin:$PATH"
export PATH="/usr/local/opt/icu4c/sbin:$PATH"













