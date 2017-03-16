# update all circonus packages
function circ-pkg-update {
    pkg fix '//circonus/*'
    pkg update -v '//circonus/*'
}

# Show the current status of all services
# Refresh after $1 seconds (default 5)
function smf-top {
    while true
    do
        clear
        svcs -ao STA,STIME,FMRI -s STIME
        sleep ${1:-5}
    done
}
