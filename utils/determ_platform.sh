#!/bin/bash
case "$(uname -s)" in
    Linux)
        echo "Linux" ;;
    CYGWIN*|MINGW32*|MSYS*)
        echo "Windows" ;;
    Darwin)
        echo "Mac" ;;
    *)
        echo "Other" ;;
esac
