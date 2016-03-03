#!/bin/bash
# LICENSE: PUBLIC DOMAIN
# inspired by http://askubuntu.com/a/309028/41454
# switch between my layouts

# If an explicit layout is provided as an argument, use it. Otherwise, select the next layout from
# the set [us, it, fr].
if [[ -n "$1" ]]; then
    setxkbmap $1
else
    layout=$(setxkbmap -query | grep "layout: " |awk '{print $2}')
    case $layout in
	us|'us(intl)'|intl)
                setxkbmap de
            ;;
        de)
                setxkbmap "us(intl)"
            ;;
    esac
fi
