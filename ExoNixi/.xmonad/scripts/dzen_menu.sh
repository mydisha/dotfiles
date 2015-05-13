#!/bin/bash
Nama1="Browser"
Exec1="chrome"
Nama2="File Manager"
Exec2="spacefm"
Nama3="Image Editor"
Exec3="gimp"
Nama4="Text Editor"
Exec4="geany"
Nama5="Change Wallpaper"
Exec5="nitrogen"
Nama6="Power Menu"
Exec6="oblogout"
Y=768
space="   "
Font="Exo 2-8"
Title="Menu"
BG=#ff4660
FG=#fcfcfc
IconTitle="/home/crucia/.xmonad/icons/menu.xbm"
(echo "^ro(0)^ib(1)$space^i($IconTitle)$space$Title" ;echo -e "\
\n^ca(1,$Exec1)$space$Nama1^pa(117)^ca()\
\n^ca(1,$Exec2)$space$Nama2^pa(117)^ca()\
\n^ca(1,$Exec3)$space$Nama3^pa(117)^ca()\
\n^ca(1,$Exec4)$space$Nama4^pa(117)^ca()\
\n^ca(1,$Exec5)$space$Nama5^pa(117)^ca()\
\n^ca(1,$Exec6)$space$Nama6^pa(117)^ca()")\
| dzen2 -y "$Y" -fn "$Font" -bg "$BG" -fg "$FG" -l 6 -tw 60 -ta l -w 120 -h 24 -m -p -e 'entertitle=;enterslave=grabkeys;leaveslave=collapse,ungrabkeys;button1=uncollapse,grabkeys;button2=togglestick;button3=;button4=scrollup;button5=scrolldown;key_Escape=ungrabkeys'
