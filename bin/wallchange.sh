#!/bin/bash
file=$(zenity --file-selection)
display -window root $file
