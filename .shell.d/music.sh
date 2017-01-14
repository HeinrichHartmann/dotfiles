function youtube-mp3 {
    (
        youtube-dl --extract-audio --audio-format mp3 --audio-quality 0 $@
    )
}

function youtube-video {
    (
        youtube-dl $@
    )
}
