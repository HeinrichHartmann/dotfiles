function fzf-man {
  bash -c "compgen -ac" | fzf --preview 'man {}'
}

function fzf-book-shelf {
  (cd ~/Shelf/Books; open "$(find . | fzf -i -e --preview 'pdftotext -l 3 {} -')" )
}

function fzf-book-archive {
  /usr/bin/osascript -e 'mount volume "smb://u16/share"'
  (cd /Volumes/share/attic/Books; push-and-open "$(fzf -i -e --preview 'pdftotext -l 3 {} -' < index.lst)")
}

function fzf-hist {
  # print -z 
  print -z $(<~/.zsh_history awk 'BEGIN{FIELDWIDTHS="2 10 3 *"} { print strftime("%F %T", $2) "\t" $4 }' | fzf --no-sort --tac | sed 's/.*	//g')
}

function fzf-git-log {
  git log --oneline | fzf --multi --preview 'git show {+1}'
}

function fzf-ag {
  ag --nobreak --nonumbers --noheading ${1:-.} | fzf --no-sort
}
