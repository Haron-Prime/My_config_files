#!/usr/bin/env zsh
# Author - Haron Prime
# License WTFPL © 2017 http://www.wtfpl.net/

alias yupg='yay --color auto -Syu'        # Synchronize with repositories before upgrading packages (AUR packages too) that are out of date on the local system.
alias ysu='yay --sucre'                   # Same as yupg, but without confirmation
alias yin='yay --color auto -S'           # Install specific package(s) from the repositories
alias yins='yay -U'                       # Install specific package not from the repositories but from a file
alias yre='yay -R'                        # Remove the specified package(s), retaining its configuration(s) and required dependencies
alias yrem='yay -Rns'                     # Remove the specified package(s), its configuration(s) and unneeded dependencies
alias yrep='yay --color auto -Si'         # Display information about a given package in the repositories
alias yreps='yay --color auto -Ss'        # Search for package(s) in the repositories
alias yloc='yay -Qi'                      # Display information about a given package in the local database
alias ylocs='yay -Qs'                     # Search for package(s) in the local database
alias ylst='yay -Qe'                      # List installed packages, even those installed from AUR (theyre tagged as "local")
alias yorph='yay -Qtd'                    # Remove orphans using yay
# Additional yay alias examples
if [[ -x `command -v abs` && -x `command -v aur` ]]; then
    alias yupd='yay --color auto -Sy && sudo abs && sudo aur'  # Update and refresh the local package, ABS and AUR databases against repositories
elif [[ -x `command -v abs` ]]; then
    alias yupd='yay --color auto -Sy && sudo abs'              # Update and refresh the local package and ABS databases against repositories
elif [[ -x `command -v aur` ]]; then
    alias aupd='yay --color auto -Sy && sudo aur'              # Update and refresh the local package and AUR databases against repositories
else
    alias yupd='yay --color auto -Sy'                          # Update and refresh the local package database against repositories
fi
alias yinsd='yay --color auto -S --asdeps'                     # Install given package(s) as dependencies of another package
alias ymir='yay --color auto -Syy'                             # Force refresh of all package lists after updating /etc/pacman.d/mirrorlist
