#!/usr/bin/env bash

a=$1
b=$2

exec delta <(jq . $a) <(jq . $b)
