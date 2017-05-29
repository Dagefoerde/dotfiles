# retrieve command cheat sheets from cheat.sh
# fish version by @tobiasreischmann
function cheat
    curl cheat.sh/$argv
end
# register completions (on-the-fly, non-cached, because the actual command won't be cached anyway
complete -c cheat -xa '(curl -s cheat.sh/:list)'
