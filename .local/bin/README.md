
All listed scripts must be in the directory `$HOME/.local/bin`
The results of all scripts are displayed in the xmobar using pipes
Some scripts use characters from [Font-Awesome](https://github.com/FortAwesome/Font-Awesome) and [Weather Icons Font](https://github.com/erikflowers/weather-icons)


### Scripts used in xmonad.h

| -------------------------- | ------------------------------------------------------------------------- |
| XMStart                    | Running programs and scripts at start xmonad                              |
| XMR                        | Restart xmonad, xmobar, gis-weather and running scripts                   |
| XMRR                       | Recompile xmonad. Restart xmonad, xmobar, gis-weather and running scripts |
| XMMWO                      | Minimize windows with XMonad.Layout.Minimize                              |
| XMMWC                      | Restore minimize windows with XMonad.Layout.Minimize                      |
| XMMPCtoggle                | `mpc toggle` and current artist/track in popup notification (dunst)       |
| XMMPCstop                  | `mpc stop` and current artist/track in popup notification (dunst)         |
| XMMPCprev                  | `mpc prev` and current artist/track in popup notification (dunst)         |
| XMMPCnext                  | `mpc next` and current artist/track in popup notification (dunst)         |
| XMGalculator               | Start/stop galculator                                                     |
| XMNotes-w                  | Write the selected text to a file (my text notes)                         |
| XMTransgui                 | Start/stop transgui                                                       |
| XMYaourt                   | Start `yaourt -Syua` in a new terminal window                             |
| XMUptimeToggle             | ON/OFF XMUptime                                                           |
| XMNetToggle                | ON/OFF XMNetSpeed                                                         |
| XMVnstat                   | `vnstat` in popup notification (dunst)                                    |
| XMVnstat-h                 | `vnstat -h` in popup notification (dunst)                                 |
| XMVnstat+h                 | `vnstat & vnstat -h` in popup notification (dunst)                        |
| MVnstat-d                  | `vnstat -d` in popup notification (dunst)                                 |
| XMTrafToggle               | ON/OFF XMTraf                                                             |
| XMCPUToggle                | ON/OFF XMCUNew                                                            |
| XMTop-cpu                  | Top-cpu apps in popup notification (dunst)                                |
| XMTempToggle               | ON/OFF XMTempAll                                                          |
| XMSensors                  | `sensors` in popup notification (dunst)                                   |
| XMMemToggle                | ON/OFF XMMemAll                                                           |
| XMTop-mem                  | Top-mem apps in popup notification (dunst)                                |
| XMdf-h                     | `df-h` in popup notification (dunst)                                      |
| XMVolToggle                | ON/OFF XMVolNew                                                           |
| XMDateToggle               | ON/OFF XMDate                                                             |
| XMToggleAll                | ON/OFF all used in xmobar scripts                                         |
| XMCleanAll                 | Clear all pipes                                                           |
| compdown                   | Shutdown computer (with zenity)                                           |
| compreboot                 | Restart computer (with zenity)                                            |
| gis-weather                | Script to run the git-version of Gis-weather                              |


### Scripts used in xmobar.hs and in other scripts


| -------------------------- | ---------------------------------------------- |
| XMUptime                   | Uptime, load average, kernel version etc |
| XMUptimeToggle             | ON/OFF XMUptime |
| XMDate                     | Date, tz etc |
| XMDateToggle               | ON/OFF XMDate |
| XMTime                     | Current time |
| XMGSimplecal               | Start/stop gsimplecal |
| XMCal                      | `cal -y` in popup notification (dunst) |
| XMGWeather                 | Current weather state in popup notification (dunst) |
| XMAccuWeather              | Open https://www.accuweather.com in default browser |
| XMGismeteo                 | Open https://www.gismeteo.ru in default browser |
| XMUpdate                   | Checking for packages updates |
| XMYaourt                   | Start `yaourt -Syua` in a new terminal window |
| XMNetTest                  | Network connection status |
| XMNetSpeed (XMNetSpeedBit) | Current speed - B/s, KiB/s, MiB/s (bps, kbps, Mbps) |
| XMNetToggle                | ON/OFF XMNetSpeed |
| XMVnstat                   | `vnstat` in popup notification (dunst) |
| XMVnstat-h                 | `vnstat -h` in popup notification (dunst) |
| XMVnstat+h                 | `vnstat & vnstat -h` in popup notification (dunst) |
| XMVnstat-m                 | `vnstat -m` in popup notification (dunst) |
| XMVnstat-d                 | `vnstat -d` in popup notification (dunst) |
| XMifconfig                 | `ifconfig` in popup notification (dunst) |
| XMTrans                    | Transmission-daemon status |
| XMTraf                     | Traffic for today |
| XMTrafToggle               | ON/OFF XMTraf |
| XMTransgui                 | Start/stop transgui |
| XMCUNew                    | CPU load & freq |
| XMCPUToggle                | ON/OFF XMCUNew |
| XMTop-cpu                  | Top-cpu 9 apps in popup notification (dunst) |
| XMHtop                     | Start/stop urxvt+htop |
| XMTempAll                  | Temperature all device |
| XMTempToggle               | ON/OFF XMTempAll |
| XMSensors                  | `sensors` in popup notification (dunst) |
| XMMemAll                   | Using RAM, SWAP and disk partitions |
| XMMemToggle                | ON/OFF XMMemAll |
| XMTop-mem                  | Top-mem 9 apps in popup notification (dunst) |
| XMFree                     | `free` in popup notification (dunst) |
| XMVolNew                   | Volume level |
| XMVolToggle                | ON/OFF XMVolNew |
| XMPlay                     | Playback (current artist/track) |
| XMPavucontrol              | Start/stop pavucontrol |
| XMMPD                      | MPD control buttons |
| XMncmpcpp                  | Open/close a new terminal window with ncmpcpp |
| XMToggleAll                | ON/OFF all used in xmobar scripts |
| XMCleanAll                 | Clear all pipes |

#### Author - Haron Prime
#### License WTFPL © 2017 [http://www.wtfpl.net/](http://www.wtfpl.net/)
