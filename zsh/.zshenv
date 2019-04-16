# Set LANG
if [[ -z "$LANG" ]]; then
  export LANG="en_US.UTF-8"
fi

# Add /usr/local/{s,}bin to path
path=($path /usr/local/bin /usr/local/sbin)

# Add pip binary directory to path
path=($HOME/.local/bin $path)
