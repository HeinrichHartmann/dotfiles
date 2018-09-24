function rec-add-contact {
    (
        set -e
        builtin cd ~/Records
        recins -t contacts \
               -f Date -v $(date +%F) \
               -f Name -v "$(echo $@)" \
               contacts.rec
        emacs contacts.rec
    )
}
