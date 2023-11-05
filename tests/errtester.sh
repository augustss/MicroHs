# Split the input file and test each snippet for the correct error message.
IFS=""
tmp=../tmp
out=$tmp/E.hs
err=$tmp/err
terr=$tmp/terr
cerr=$tmp/cerr
comp=../bin/mhs
read -r line
while [ "$line" != "END" ]; do
    echo > $out
    while true; do
        if [ "$line" = "-----" ]; then
            break
        fi
        echo "$line" >> $out
        read -r line
    done
    echo > $terr
    read -r line
    while true; do
        if [ "$line" = "=====" ]; then
            break
        fi
        echo "$line" >> $terr
        read -r line
    done
    read -r line

    #echo "Trying:"
    #cat $out
    #echo "---"
    #cat $err
    #echo "==="
    #echo "next: $line"
    sed -e '/^ *$/d' $terr > $err
    $comp -i../lib -i../tmp E 2>&1 | sed -e '/CallStack/,$d' -e '/^XX/d' > $cerr
    diff $err $cerr || exit 1
done
