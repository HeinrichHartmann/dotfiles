if [ -d $HOME/perl5/perlbrew ]
then
    export PERLBREW_ROOT=$HOME/perl5/perlbrew
    source ${PERLBREW_ROOT}/etc/bashrc
    perlbrew use 5.24.0
fi
