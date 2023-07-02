#!/bin/sh
( echo "{-# LINE 1 \"$1\" #-}" ; sed -e 's/--X//' $2 ) > $3
