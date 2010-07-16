#!/bin/sh

exec erl -pa deps/*/ebin -pa ebin -sname simplegeo_shell \
    -s crypto -s ssl -s lhttpc -s simplegeo -s reloader \
    -simplegeo consumer_key \"$1\" -simplegeo consumer_secret \"$2\"
