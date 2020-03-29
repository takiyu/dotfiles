#!/bin/sh

rsync_option_up=' -auvz --copy-links --delete --update'
# rsync_option_down=' -auvz --copy-links --delete --update'
rsync_option_down=' -auvz --copy-links --update'

sources="Class \
		 Essay \
		 Documents \
		 Downloads \
		 MPLABXProjects \
		 bin \
		 Projects "
# 		 workspace \


# argc < 2 -> exit
if [ $# -lt 2 ]; then
	echo 'argv[0] == l/g'
	echo 'argv[1] == up/down'
	exit 1
fi

# set server
if [ $1 = 'l' ]; then
	server='t-server-local'
elif [ $1 = 'g' ]; then
	server='t-server-global'
else
	echo 'invalid args'
fi
# set mode (up or down)
mode=$2

# argc < 3 (1 or 2 arg) || arcv(3) != 'run'  ->  Test Mode
if [ $# -lt 3 ] || [ $3 != 'run' ]; then
	echo '====== Test Mode ======'
	rsync_option_up="$rsync_option_up -n"
	rsync_option_down="$rsync_option_down -n"
fi


if [ $mode = 'up' ]; then
	echo "<<< Upload >>>"
	# Upload
	for item in $sources; do
		echo "----- up : $item -----"
		rsync $rsync_option_up "$HOME/$item" "$server:/mnt/md126/FileStorage/ActiveStore"
	done
elif [ $mode = 'down' ]; then
	echo "<<< Download >>>"
	# Download
	for item in $sources; do
		echo "----- down : $item -----"
		rsync $rsync_option_down "$server:/mnt/md126/FileStorage/ActiveStore/$item" "$HOME/"
	done
else
	echo 'invalid args'
fi
