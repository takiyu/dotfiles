case "$(uname -s)" in
    Linux)
        echo "Linux" ;;
    CYGWIN*|MINGW32*|MINGW64*|MSYS*)
        echo "Windows" ;;
    Darwin)
        echo "Mac" ;;
    *)
        echo "Other" ;;
esac
