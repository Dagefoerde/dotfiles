# Path to Oh My Fish install.
set -gx OMF_PATH "/home/j_dage01/.local/share/omf"

# Customize Oh My Fish configuration path.
#set -gx OMF_CONFIG "/home/j_dage01/.config/omf"

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

set -g fish_key_bindings fish_vi_key_bindings

# own addons
source "$HOME/.config/fish/own_functions/aliases.fish"
source "$HOME/.config/fish/own_functions/git.fish"
source "$HOME/.config/fish/own_functions/docker.fish"

