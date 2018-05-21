#!/usr/bin/env zsh
# Author - Haron Prime
# License WTFPL © 2017 http://www.wtfpl.net/

alias aupg='aurman --color auto -Syu'        # Synchronize with repositories before upgrading packages (AUR packages too) that are out of date on the local system.
# alias asu='aurman --sucre'                   # Same as aupg, but without confirmation
alias ain='aurman --color auto -S'           # Install specific package(s) from the repositories
alias ains='aurman -U'                       # Install specific package not from the repositories but from a file
alias are='aurman -R'                        # Remove the specified package(s), retaining its configuration(s) and required dependencies
alias arem='aurman -Rns'                     # Remove the specified package(s), its configuration(s) and unneeded dependencies
alias arep='aurman --color auto -Si'         # Display information about a given package in the repositories
alias areps='aurman --color auto -Ss'        # Search for package(s) in the repositories
alias aloc='aurman -Qi'                      # Display information about a given package in the local database
alias alocs='aurman -Qs'                     # Search for package(s) in the local database
alias alst='aurman -Qe'                      # List installed packages, even those installed from AUR (theyre tagged as "local")
alias aorph='aurman -Qtd'                    # Remove orphans using aurman
# Additional aurman alias examples
if [[ -x `command -v abs` && -x `command -v aur` ]]; then
    alias aupd='aurman --color auto -Sy && sudo abs && sudo aur'  # Update and refresh the local package, ABS and AUR databases against repositories
elif [[ -x `command -v abs` ]]; then
    alias aupd='aurman --color auto -Sy && sudo abs'              # Update and refresh the local package and ABS databases against repositories
elif [[ -x `command -v aur` ]]; then
    alias aupd='aurman --color auto -Sy && sudo aur'              # Update and refresh the local package and AUR databases against repositories
else
    alias aupd='aurman --color auto -Sy'                          # Update and refresh the local package database against repositories
fi
alias ainsd='aurman --color auto -S --asdeps'                     # Install given package(s) as dependencies of another package
alias amir='aurman --color auto -Syy'                             # Force refresh of all package lists after updating /etc/pacman.d/mirrorlist
