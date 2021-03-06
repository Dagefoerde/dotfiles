# Path to Oh My Fish install.
set -gx OMF_PATH "$HOME/.local/share/omf"

# Customize Oh My Fish configuration path.
set -gx OMF_CONFIG "$HOME/.config/omf"

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

set -g fish_key_bindings fish_default_key_bindings # switch to hybrid in fish >> 2.4.0

# own addons
#source "$HOME/.config/fish/solarized.fish"
source "$HOME/.config/fish/own_functions/aliases.fish"
source "$HOME/.config/fish/own_functions/git.fish"
source "$HOME/.config/fish/own_functions/docker.fish"
source "$HOME/.config/fish/own_functions/mulidev.fish"
source "$HOME/.config/fish/own_functions/cheat.fish"

