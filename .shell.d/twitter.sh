function tweet() {
    twurl -d "status=$*" /1.1/statuses/update.json | jq .
}
