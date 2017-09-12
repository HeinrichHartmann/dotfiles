function git-head {
    printf '%s %s@%s\n' \
           `git config --get remote.origin.url` \
           `git symbolic-ref --short HEAD` \
           `git rev-parse HEAD | cut -c 1-6`
}

function git2ps {
    a2ps --header='' \
         --left-title='$d' \
         --center-title='$n' \
         --right-title='$Q' \
         --left-footer="`git-head`" \
         --footer="" \
         --right-footer="`date +'%F %H:%M:%S'`" \
         --line-numbers=0 \
         --lines-per-page=80 \
         --highlight-level=none \
         --medium=A4 \
         --sides=2 \
         --file-align=sheet \
         --output=src.ps \
         $@ &&
     ps2pdf -sPAPERSIZE=a4 -dOptimize=true -dEmbedAllFonts=true src.ps
}

function git2ps-tuned {
    git2ps --portrait \
           --columns=2 \
           --chars-per-line=80 \
           --tabsize 4  \
           --line-numbers=0 \
           --margin=0 \
           --output=src.ps \
           $@
}
