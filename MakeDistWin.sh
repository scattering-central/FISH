#!/usr/bin/sh

# make a distributable zip file (minus source code)
# yes, this is a bash script that needs to run in Cygwin or similar

mkdir DistWin
cp -rpv bin DistWin
cp -rpv lib DistWin
cp -rpv work DistWin
cp -pv fish.exe DistWin
cp -pv Release_notes.html DistWin
rm -rfv DistWin/lib/.svn
rm -rfv DistWin/work/.svn
rm -rfv DistWin/work/fishlog.txt
cd DistWin
zip -r ../Fish_win.zip *
