# set this in your /etc/hosts
YTDL_HOST=ytdl_host

function YTDL {
  FOLDER="~/Attic/Videos"
  OPTS="--format '"'best[height<=760]'"' --recode-video mp4 --restrict-filenames -o '%(upload_date)s CHANNEL=%(uploader)s TITLE=%(title)s.%(ext)s'"
  POST_CMD=""
  PRE_CMD=""
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
  printf '%s\n' "$CMD" | ssh YTDL_HOST -- bash -x
}
