MP=paths/Paths_MicroHs.hs
mv $MP $MP.bak
grep -v '^version =' $MP.bak > $MP
grep '^version:' MicroHs.cabal | sed -e 's/: */ = makeVersion [/' -e 's/\./,/g' -e 's/$/]/' >> $MP
#
MK=Makefile
mv $MK $MK.bak
vers=`grep '^version:' MicroHs.cabal | sed -e 's/version: *//'`
#echo $vers
sed -e "s/^VERSION=.*/VERSION=$vers/" $MK.bak > $MK
