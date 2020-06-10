function pwgen {
  openssl rand -base64 ${1:-12}
}

function zip-encrypt {
  (
    export PASS="$(pwgen)"
    zip --encrypt --password "$PASS" "$1.zip" "$1"
    printf "%s\n" "$PASS" > "$1.pwd"
    printf "Password: %s\n" "$PASS" > /dev/stderr
  )
}
