#!/bin/bash

# List sinks with pactl and get theirs names
SINKS=$(pactl list short sinks | awk '{print $2}')

# Create an assoc array for saving [sink_description]=<sink_name>
declare -A SINKS_MAP

# Get the sinks descrption and add to the sink assoc array
while read SINK_NAME; do
    # Get the sinks full info and match only the current sink
	# Try to find the description
	# Split by ':' delimiter
	# Get the last item from the split
	# Remove the spaces
    SINK_DESCRIPTION=$( \
	    pactl list sinks | grep -n2 "${SINK_NAME}" \
					     | grep "Description" \
					     | tr ":" "\n" \
					     | tail -n1 \
					     | sed -e 's/^[[:space:]]*//' \
    )
    
    # Add to the assoc array
    SINKS_MAP[$SINK_DESCRIPTION]=$SINK_NAME
done < <(pactl list short sinks | awk '{print $2}')  # Use process subsitution: https://stackoverflow.com/a/9985310/10106533

# Create the Rofi options
ROFI_DELIMITER="|"
ROFI_OPTIONS=""
for key in "${!SINKS_MAP[@]}" 
do 
	ROFI_OPTIONS+="> ${key}${ROFI_DELIMITER}"
done

# Remove the last delimiter
ROFI_OPTIONS=${ROFI_OPTIONS::-1}
ROFI_LINES=${#SINKS_MAP[@]}

# Get the sink description from user
CHOSEN_SINK_DESCRIPTION=$( \
	rofi -sep "|" \
	     -dmenu \
	     -i \
	     -p 'Select' \
	     -location 3 \
	     -columns 1 \
		 -hide-scrollbar \
		 -xoffset "-153" -yoffset 30 \
		 -width 20 -padding 20 \
		 -line-padding 4  -lines ${ROFI_LINES} \
		 <<< "${ROFI_OPTIONS}" | sed 's/> //g')

# Verify if something was chosen
if [[ -z ${CHOSEN_SINK_DESCRIPTION} ]]; then
	exit 0
fi

# Get the sink name
CHOSEN_SINK_NAME=${SINKS_MAP[${CHOSEN_SINK_DESCRIPTION}]}

# Get the sink ID
CHOSEN_SINK_ID=$(pactl list short sinks | grep "${CHOSEN_SINK_NAME}" | awk '{print $1}')

# Get sinks inputs and move them to the chosen sink
pactl list short sink-inputs | while read SINK_INPUT; do
	SINK_INPUT_ID=$(echo ${SINK_INPUT} | awk '{print $1}')
	pactl move-sink-input "${SINK_INPUT_ID}" "${CHOSEN_SINK_ID}"
done

# Set the chosen sink to the default
pacmd set-default-sink ${CHOSEN_SINK_NAME}

polybar-msg cmd restart
