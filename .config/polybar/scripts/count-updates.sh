#!/bin/sh

pkgs_to_update=`checkupdates | wc -l`
# pid=$!

# trap "kill $pid 2> /dev/null" EXIT

# while kill -0 $pid 2> /dev/null; do
#     echo "󰓦"
#     sleep 1
# done

if [ $pkgs_to_update -gt 0 ] ; then
    updates=$pkgs_to_update
else
    updates=0
fi

if [ "$updates" -gt 0 ]; then
    echo "󰏖 $updates"
else
    echo "󰏗 Nothing"
fi

# trap - EXIT