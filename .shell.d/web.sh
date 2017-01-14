function urlencode {
    perl -MURI::Escape -ne 'chomp; print uri_escape($_), "\n"'
}
