function git-head {
    printf '%s %s@%s\n' \
           `git config --get remote.origin.url` \
           `git symbolic-ref --short HEAD` \
           `git rev-parse HEAD | cut -c 1-6`
}

function git2ps {
    # --left-title='lt #?2||$e $T|' \
    # --center-title='ct #?1|$t1|$n|' \
    # --left-footer='lf #?l!%E!#?v|%E|%s./%s#|!' \
    # --footer='f #?l|#!s-$f-, -||' \
    # --right-footer='#?l!%s./%s#!#?v|%s./%s#|%E|!' \
    # --right-footer='%s./%s#' \
    a2ps --header='' \
         --left-title='$d' \
         --center-title='$n' \
         --right-title='$Q' \
         --left-footer="`git-head`" \
         --footer="" \
         --right-footer="`date +'%F %H:%M:%S'`" \
         --output=src.ps \
         $@
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
