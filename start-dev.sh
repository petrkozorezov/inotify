#!/bin/sh

make

cd `dirname $0`
exec erl -name some -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s inotify