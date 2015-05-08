cd bin
rm ~/karel2014/bin.zip
find . | sort | zip -9 -D ~/karel2014/bin.zip -@
cd ~
find karel2014/ | sort | zip -9 -D ~/karel2014.zip -@
mv -t ~/Dropbox/Public karel2014.zip
cd ~/workspace/karel
