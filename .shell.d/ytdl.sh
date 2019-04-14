function YTDL {
  FOLDER="~/YTDL"
  OPTS="--format '"'best[height<=760]'"' --recode-video mp4 --restrict-filenames -o '%(upload_date)s CHANNEL=%(uploader)s TITLE=%(title)s.%(ext)s'"
  POST_CMD=""
  PRE_CMD=""
  REMOTE=""
  while [[ "$#" -gt 0 ]]
  do
    echo "> $1"
    case $1 in
      ps)
        ssh YTDL_HOST -- "ps axw -o 'pid,lstart,time,command' | grep youtube-dl | grep -v grep"
        return
        ;;
      -p|--podcast)
        shift
        echo "Saving video to podcast directory"
        FOLDER="~/Shelf/Podcasts"
        ;;
      -d|--daemon)
        shift
        echo "Daemonizing"
        PRE_CMD="$PRE_CMD nohup"
        POST_CMD="& $POST_CMD"
        OPTS="$OPTS --quiet"
        ;;
      -s|--subfolder)
        shift
        PRE_CMD="mkdir -p $1; cd $1; $PRE_CMD"
        shift
        ;;
      -r|--remote)
        shift
        REMOTE=$1
        shift
        ;;
      *)
        break
        ;;
    esac
  done
  URL=$1
  shift
  [[ $URL ]] || { echo "No URL provided"; return }
  OPTS="$OPTS $*"
  CMD="cd $FOLDER; $PRE_CMD youtube-dl $OPTS $URL $POST_CMD"
  if [[ -n "$REMOTE" ]]
  then
    printf '%s\n' "$CMD" | ssh "$REMOTE" -- bash -x
  else
    printf '%s\n' "$CMD" | bash -x
  fi
}
