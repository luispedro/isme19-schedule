#!/usr/bin/env bash

set -ev

mv -i get-schedule.jugdata get-schedule.jugdata_$(date "+%Y-%m-%d_%H%M")
jug execute get-schedule.py
git difftool --extcmd $PWD/json-diff.sh
