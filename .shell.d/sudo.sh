# sudo edit
function sudo_edit() {
    FILE=$1
    [[ $# -ne 1 || ! -e $FILE ]] && {
        echo "Usage: $0 <filename>"
        return 1 # FAIL
    }
    TMP_FILE=$(mktemp)
    cp "$FILE" "$TMP_FILE"
    sudoedit "$FILE"
    printf "Diff:\n"
    diff "$FILE" "$TMP_FILE"
    return 0
}
