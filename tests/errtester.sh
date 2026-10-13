# Split the input file and test each snippet for the correct error message.
IFS=""
tmp=../tmp
out=$tmp/E.hs
err=$tmp/err
terr=$tmp/terr
cerr=$tmp/cerr
comp=$1; shift
# Use the compilation cache when it exists; it makes each snippet much faster.
if [ -f .mhscache ]; then cache=-CR; else cache=; fi
read -r line

mkdir -p $tmp

while [ "$line" != "END" ]; do
    echo > $out
    while true; do
        if [ "$line" = "-----" ]; then
            break
        fi
        printf "%s\n" "$line" >> $out
        read -r line
    done
    echo > $terr
    read -r line
    while true; do
        if [ "$line" = "=====" ]; then
            break
        fi
        printf "%s\n" "$line" >> $terr
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
    $comp $* $cache -i../lib -i../tmp E 2>&1 | sed -e 's/^gmhs/mhs/' -e '/CallStack/,$d' -e '/^XX/d' -e 's/^.*mhs: error:/mhs:/' -e 's/^.*mhs: uncaught exception: error:/mhs:/' -e 's/^mhs: //' -e '/Uncaught exception/d' -e '/^ *[0-9]* |/d' -e '/^ *$/d'  > $cerr
    if diff $err $cerr
    then
        :
    else
        cat ../tmp/E.hs
        exit 1
    fi
done
