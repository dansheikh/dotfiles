#!/usr/bin/env bash

printf "Installing Visual Studio Code extensions...\n"

while read line; do code --install-extension "$line"; done < extensions.txt

printf "Installing Visual Studio Code extensions complete...\n"
