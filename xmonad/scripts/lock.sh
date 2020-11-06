#!/bin/sh

source ~/.cache/wal/colors.sh

WRONG="#880000FF"
TRANSPARENT="#33333333"

i3lock \
--insidevercolor=${TRANSPARENT}   \
--ringvercolor=${foreground} \
\
--insidewrongcolor=${TRANSPARENT} \
--ringwrongcolor=${WRONG}    \
\
--insidecolor=${TRANSPARENT} \
--ringcolor=${foreground}    \
--linecolor=${color1}        \
--separatorcolor=${color1}   \
\
--verifcolor=${color1}       \
--wrongcolor=${WRONG}        \
--timecolor=${foreground}    \
--datecolor=${foreground}    \
--layoutcolor=${foreground}  \
--keyhlcolor=${color2}       \
--bshlcolor=${background}         \
\
--screen 1                   \
--blur 5                     \
--clock                      \
--indicator                  \
--timestr="%H:%M:%S"         \
--datestr="%A, %m %Y"        \
--keylayout 1                \

# --veriftext="Drinking verification can..."
# --wrongtext="Nope!"
# --textsize=20
# --modsize=10
# --timefont=comic-sans
# --datefo
