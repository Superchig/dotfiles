#!/bin/sh

case "$1" in
    *.jpg) exiv2 "$1";;
    *.png) exiv2 "$1";;
    *.gif) exiv2 "$1";;
    *.tar*) tar tf "$1";;
    *.zip) unzip -l "$1";;
    *.rar) unrar l "$1";;
    *.7z) 7z l "$1";;
    *.pdf) pdftotext "$1" -;;
    *.mp4) mediainfo "$1";;
    *.mkv) mediainfo "$1";;
    *) highlight -O ansi "$1" || cat "$1";;
esac
