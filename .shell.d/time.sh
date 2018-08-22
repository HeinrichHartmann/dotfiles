#
# Timestamp
#

if command -v gdate >/dev/null 2>&1
then
  DATE=gdate
else
  DATE=date
fi

alias iso-datetime='date +"%FT%T%Z"'
alias iso-date='date +"%F"'
alias epoch='date +%s'

# Convert ISO Date to epoch
function hh-date-to-epoch { $DATE -d"$1" +%s }
function hh-eopoch-to-date { $DATE -d @"$1" -Isec }
