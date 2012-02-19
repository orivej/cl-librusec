for i in ../separated/*; do lndir $i/ .; done
find -L ../separated -type f -print0 | xargs -0 ln -s -t .
