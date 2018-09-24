function docker-dev {
    docker run --rm -it --privileged \
           -v ~/.ssh:/root/.ssh \
           -v $(pwd):/mnt/cwd \
           -v /:/mnt/root \
           hh-dev:latest $@
}
