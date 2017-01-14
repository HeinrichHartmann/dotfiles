alias chrome="google-chrome"
alias chrome-private='google-chrome --profile-directory="Default"'
alias chrome-work='google-chrome --profile-directory="Profile 2"'

function google() {
    chrome "https://www.google.com/#q=$@"
}


alias circ-jira='chrome-work --app="https://circonus.atlassian.net/secure/RapidBoard.jspa?rapidView=3" 2>&1 > /dev/null &'
alias circ-slack='chrome-work --app="https://circonus.slack.com/messages/general/" 2>&1 > /dev/null &'
alias circ-cal='chrome-work --app="https://calendar.google.com/calendar/render?pli=1#h" 2>&1 > /dev/null &'
alias circ-mail='chrome-work --app="https://mail.google.com/mail/u/0/#inbox" 2>&1 > /dev/null &'
alias circ-freshdesk='chrome-work --app="https://circonus.freshdesk.com/helpdesk/dashboard" 2>&1 > /dev/null &'

function circ-env() {
    circ-slack
    circ-jira
    circ-cal
    circ-mail
}
