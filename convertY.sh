#!/bin/sh
( echo "{-# LINE 1 \"$1\" #-}" ; sed -e 's/--Y//' $2 ) > $3
