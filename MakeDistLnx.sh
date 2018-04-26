#!/usr/bin/sh

# make a distributable tar file (minus source code)

mkdir DistLnx
cp -rpv bin DistLnx
cp -rpv lib DistLnx
cp -rpv work DistLnx
cp -pv fish DistLnx
cp -pv Release_notes.html DistLnx
rm -rfv DistLnx/lib/.svn
rm -rfv DistLnx/work/.svn
rm -rfv DistLnx/work/fishlog.txt
cd DistLnx
chmod a+x fish
tar cfz ../Fish_linux.tar.gz *

