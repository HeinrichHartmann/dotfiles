function libcat {
    echo "#include <$1>" | gcc -E - | egrep -v "^$" | egrep -v "^#"
}
