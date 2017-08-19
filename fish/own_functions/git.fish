# git shortcuts
abbr -a g git
abbr -a gd git diff
abbr -a gc git commit -v
abbr -a gcf git commit -v --fixup
abbr -a gcp git cherry-pick 
abbr -a gsb git status -sb
abbr -a gs git status
abbr -a gst "git status -s | while read mode file; echo \$mode \$file (stat -c \\%y \$file); end;"
abbr -a ga git add
abbr -a gaa git add --all
abbr -a gco git checkout
abbr -a gpr git pull --rebase
abbr -a gl "git log --graph --pretty=format:'%G? %Cred%h %Cgreen[%ai] %Cblue<%an>%Cgreen%C(bold)%d%Creset %s %Creset'"
abbr -a gr git rebase
abbr -a grc git rebase --continue
abbr -a gps git push
abbr -a gpsf git push --force-with-lease

# Function that opens the GitHub/GitLab repository corresponding to the current folder.
# Inspired by the post of https://dev.to/shayde/open-the-github-project-page-of-a-repo-from-terminal
# Adapted from the comment https://dev.to/shayde/open-the-github-project-page-of-a-repo-from-terminal/comments/5m2
function github
	if not git rev-parse
		echo "ERROR: This isn't a git repository or subdirectory"; and return (false);
	end

	set -l git_url (git config --get remote.origin.url)
	set -l git_domain (echo $git_url | awk -v FS="(@|:)" '{print $2}')
	set -l git_branch (git rev-parse --abbrev-ref HEAD 2>/dev/null)

	switch $git_url
		case 'https://*'
			set -l repo (string replace -r ".git\$" "" -- $repo) # strip trailing .git
			set url $git_domain/$repo/tree/$git_branch
		case 'git@*'
			set -l repo (string replace -r ".*:" "" -- $git_url) # strip hostname
			set -l repo (string replace -r ".git\$" "" -- $repo) # strip trailing .git
			set url "https://$git_domain/$repo/tree/$git_branch"
		case '*'
			echo "ERROR: Remote origin is invalid"; and return (false);
	end
	sensible-browser $url
end

