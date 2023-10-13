# Split the input file and test each snippet for the correct error message.
IFS=""
tmp=../tmp
out=$tmp/E.hs
err=$tmp/err
cerr=$tmp/cerr
comp=../bin/mhs
read -r line
while [ "$line" != "END" ]; do
    echo "module E(module E) where" > $out
    echo "import Prelude" >> $out
    while true; do
        if [ "$line" = "-----" ]; then
            break
        fi
        echo "$line" >> $out
        read -r line
    done
    cp /dev/null $err
    read -r line
    while true; do
        if [ "$line" = "=====" ]; then
            break
        fi
        echo "$line" >> $err
        read -r line
    done
    read -r line

    #echo "Trying:"
    #cat $out
    #echo "---"
    #cat $err
    #echo "==="
    #echo "next: $line"
    $comp -i../lib -i../tmp E 2>&1 | sed -e '/CallStack/,$d' > $cerr
    diff $err $cerr
done
