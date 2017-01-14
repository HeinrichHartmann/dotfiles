# wrapper for userlevel systemd commands

alias uctl='systemctl --user'

alias uctl-refresh='uctl daemon-reload && uctl list-unit-files'

alias uctl-list-service='uctl list-unit-files --type=service'
alias uctl-list-timer='uctl list-unit-files --type=timer'

alias uctl-start='uctl start'
alias uctl-stop='uctl stop'
alias uctl-restart='uctl restart'
alias uctl-status='uctl status'
