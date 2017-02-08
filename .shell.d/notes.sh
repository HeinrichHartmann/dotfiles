NOTE_ROOT="$HOME/git/notes"

function note-add {
    (   # cd only in subshell
        set -e
        cd "$NOTE_ROOT"

        filename="$(date +"%Y-%m-%d")_${1%%.md}.md"
        if [[ -e $filename ]]
        then
            println "File already exists: $filename"
        else
            println "Creating $filename"
            println "# $(date +"%Y-%m-%d") $1" > "$filename"
        fi

        $EDITOR "$filename"

        git diff
        read -n 1 -p "Save to git? (Y/n/t)" answer
        case $answer in
            n|N)
                println "File not saved."
                ;;
            r|R)
                println "Trashing file"
                trash "$filename"
                ;;
            *)
                git add "$filename"
                git commit -m "Added note $filename"
                git push
                ;;
        esac
    )
}
