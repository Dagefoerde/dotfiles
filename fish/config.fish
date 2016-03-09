# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish

# Theme
Theme "agnoster"
#Theme "budspencer" -- requires vimode :(

# All built-in plugins can be found at ~/.oh-my-fish/plugins/
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Enable plugins by adding their name separated by a space to the line below.
#set fish_plugins theme
#Plugin "theme"

# Path to your custom folder (default path is ~/.oh-my-fish/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# own addons
. "$HOME/.config/fish/functions/aliases.fish"

#if test -f /home/likewise-open/WIWI/j_dage01/apps/autojump/share/autojump/autojump.fish;
#  . /home/likewise-open/WIWI/j_dage01/apps/autojump/share/autojump/autojump.fish;
#end
