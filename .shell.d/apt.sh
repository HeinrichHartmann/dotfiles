# APT
#
function pkg-history(){
      case "$1" in
        install)
              cat /var/log/dpkg.log | grep 'install '
              ;;
        upgrade|remove)
              cat /var/log/dpkg.log | grep $1
              ;;
        rollback)
              cat /var/log/dpkg.log | grep upgrade | \
                  grep "$2" -A10000000 | \
                  grep "$3" -B10000000 | \
                  awk '{print $4"="$5}'
              ;;
        *)
              cat /var/log/dpkg.log
              ;;
      esac
}

function pkg-tail(){
    cat /var/log/dpkg.log | grep 'install ' | tail -n $1
}

alias pkg-version="apt-cache policy"

# get package index from repositories
alias pkg-update="sudo apt-get update" # update package repository

# inspect local package indices
alias pkg-search="apt-cache search" # search for packages matching a keyword
alias pkg-list-all="apt-cache pkgnames" # list all packages availabe from repositories
alias pkg-info="apt-cache show" # print information about a package

# install package
alias pkg-install="sudo apt-get install"

# inspect loacl packages
alias pkg-list="dpkg --get-selections | cut -f 1" # list all installed packages
alias pkg-list-installed="aptitude search '~i!~M'" # list all explicity installed packages
alias pkg-contents="dpkg -L" # show contensts of an installed package package



