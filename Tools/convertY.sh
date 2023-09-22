#!/bin/sh
( echo "{-# LINE 1 \"$1\" #-}" ; sed -e 's/--[YW]//' $2 ) > $3
