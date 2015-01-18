for i in /home/uj/.avfs/mnt/data/torrents/b/Librusec/*.zip; do ln -s "${i}#" `basename $i .zip`; done
