#!/bin/sh

source ~/.cache/wal/colors.sh

WRONG="#880000bb"

i3lock \
--insidevercolor=${foreground}   \
--ringvercolor=${foreground} \
\
--insidewrongcolor=${color1} \
--ringwrongcolor=${WRONG}    \
\
--insidecolor=${color1}      \
--ringcolor=${foreground}    \
--linecolor=${color1}        \
--separatorcolor=${color1}   \
\
--verifcolor=${color1}       \
--wrongcolor=${WRONG}        \
--timecolor=${foreground}    \
--datecolor=${foreground}    \
--layoutcolor=${foreground}  \
--keyhlcolor=${WRONG}        \
--bshlcolor=${WRONG}         \
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
