ret=`command -v "$@"`
if [ -n "$ret" ]; then
    echo 'exist'
else
    echo 'not exist'
fi
