#!/bin/sh

source ~/.cache/wal/colors.sh

WRONG="#880000FF"
TRANSPARENT="#33333333"

i3lock \
--insidever-color=${TRANSPARENT}   \
--ringver-color=${foreground} \
\
--insidewrong-color=${TRANSPARENT} \
--ringwrong-color=${WRONG}    \
\
--inside-color=${TRANSPARENT} \
--ring-color=${foreground}    \
--line-color=${color1}        \
--separator-color=${color1}   \
\
--verif-color=${color1}       \
--wrong-color=${WRONG}        \
--time-color=${foreground}    \
--date-color=${foreground}    \
--layout-color=${foreground}  \
--keyhl-color=${color2}       \
--bshl-color=${background}         \
\
--screen 1                   \
--blur 5                     \
--clock                      \
--indicator                  \
--time-str="%H:%M:%S"         \
--date-str="%A, %m %Y"        \
--keylayout 1                \

# --veriftext="Drinking verification can..."
# --wrongtext="Nope!"
# --textsize=20
# --modsize=10
# --timefont=comic-sans
# --datefo
