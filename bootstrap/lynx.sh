mkdir -p scratch/lynx && cd scratch
[ -e lynx-cur.tar.gz ] || wget https://invisible-mirror.net/archives/lynx/tarballs/lynx-cur.tar.gz 
tar -C lynx -xzvf lynx-cur.tar.gz
cd lynx/lynx*
./configure --prefix=$$HOME && make
