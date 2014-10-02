# 隠しファイルのみ取得してリンクを貼る
for dotfile in .?*; do
    case $dotfile in
	..)
		continue
		;;
	.git)
		continue
		;;
	.gitignore)
		continue
		;;
	.vim) #念の為
		continue
		;;
	#それ以外はシンボリックリンクを張る
	*)
		echo ">シンボリックリンクを作成:$dotfile"
		ln -s "$PWD/$dotfile" $HOME
		;;
    esac
done

# .vimディレクトリを作成
mkdir .vim

# 元からある.vimを移動
mv $HOME/.vim $HOME/_vim

# シンボリックリンクを貼る
echo ">シンボリックリンクを作成:.vim"
ln -s $PWD/.vim $HOME

# 成功したらneobundleの導入
echo ">NeoBundleをインストール"
git clone https://github.com/Shougo/neobundle.vim $PWD/.vim/bundle/neobundle.vim

# プラグインインストールして終了(起動時に聞かれるのでyを入力)
echo ">vimを起動:pluginをinstallでyを入力"
vim -c :q
