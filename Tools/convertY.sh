#!/bin/sh
( echo "{-# LINE 1 \"$1\" #-}" ; sed -e 's/--Y//' -e '/--Z/d' $2 ) > $3
