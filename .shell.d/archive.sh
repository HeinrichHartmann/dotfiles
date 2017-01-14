#
# scanner pdf handling
#

DOC_ROOT="$HOME/Documents"

function stamp {
    A=$1
    B="$(dirname "$1")/$(date -I)_$(basename "$1")"
    if [[ -e $A ]] && [[ ! -e $B ]]
    then
        mv "$A" "$B" || exit 1
        echo "$B"
    else
        printf "%s\n" "Could not stamp $A."
        return -1
    fi
}

function archive {
    IN="$1"
    if [[ ! -e $IN ]]
    then
        return -1
    else
        OUT_NAME="$2"
        if [[ -z $OUT_NAME ]]
        then
            OUT="$DOC_ROOT/$(date -I)_$(basename "$IN")"
        else
            OUT="$DOC_ROOT/$(date -I)_$OUT_NAME"
        fi
        mv --interactive "$IN" "$OUT"
        echo "$OUT"
    fi
}

function archive-note {
    if [[ -z $1 ]]
    then
        println "Usage: $0 <name of note>"
        return 1
    fi
    filename="${DOC_ROOT}/$(date -I)_${@}.txt"
    if [[ -e $filename ]]
    then
        println "File already exists: $filename"
    else
        println "Creating $filename"
        println "# $(date -I) $1" > "$filename"
    fi
    $EDITOR "$filename"
    echo "--- Saved note: $filename ---"
    cat "$filename"
    echo "---"
}

function archive-raw {
    filename="${DOC_ROOT}/$(date -I)_$1"
    if [[ -e $filename ]]
    then
        println "File already exists: $filename"
        return 1
    else
        println "Writing to $filename"
        cat > "$filename"
    fi
    stat "$filename"
}
