# Path to Oh My Fish install.
set -gx OMF_PATH "/home/likewise-open/WIWI/j_dage01/.local/share/omf"

# Customize Oh My Fish configuration path.
#set -gx OMF_CONFIG "/home/likewise-open/WIWI/j_dage01/.config/omf"

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

# own addons
source "$HOME/.config/fish/own_functions/aliases.fish"
