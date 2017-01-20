# git shortcuts
abbr -a g git
abbr -a gd git diff
abbr -a gc git commit -v
abbr -a gcm git commit -m
abbr -a gsb git status -sb
abbr -a gst "git status -s | while read mode file; echo \$mode \$file (stat -c \\%y \$file); end;"
abbr -a ga git add
abbr -a gco git checkout
abbr -a gpr git pull --rebase
abbr -a gl "git log --graph --all --pretty=format:'%G? %Cred%h %Cgreen[%ci] %Cblue<%an>%Cgreen%C(bold)%d%Creset %s %Creset'"



