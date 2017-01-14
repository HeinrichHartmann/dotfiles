function torch {
    (
        . /Users/hartmann/git/torch/install/bin/torch-activate
        if [ $# -eq 0 ]
        then
            rlwrap luajit
        else
            $@
        fi
    )
}
