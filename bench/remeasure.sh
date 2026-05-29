#!/bin/sh
# Re-measure OPT2-only (post-Concur-fix) vs base. Pinned, best-of-N.
cd /home/matt/Projects/microhs3
PIN="taskset -c 2"
SC="+RTS -rgmhs.comb -RTS -imhs -isrc -ilib -o/tmp/o.comb MicroHs.Main"

echo "=== MICRO suite (best-of-8 per bench, ms) ==="
for ev in bin/mhseval.base bin/mhseval; do
  echo "-- $ev --"
  for r in 1 2 3 4 5 6 7 8; do $PIN ./$ev +RTS -rBench.comb -RTS 2>/dev/null; done | \
    awk '/nfib32:|tak:|sumTo:|churn:/ {n=$1;gsub(/:/,"",n);
         for(i=1;i<=NF;i++) if($i~/^\(/){m=$i;gsub(/[()]/,"",m)}
         if(!(n in b)||m+0<b[n])b[n]=m+0}
         END{s=0;for(k in b){printf "  %-8s %5d\n",k,b[k];s+=b[k]} printf "  %-8s %5d\n","SUM",s}'
done

echo ""
echo "=== SELF-COMPILE (mhseval running gmhs.comb -> compile MicroHs.Main; best of 3) ==="
for ev in bin/mhseval.base bin/mhseval; do
  best=99999; rss=0
  for i in 1 2 3; do
    out=$(/usr/bin/time -f "%e %M" $PIN ./$ev $SC 2>&1 >/dev/null | tail -1)
    e=$(echo "$out" | awk '{print $1}'); m=$(echo "$out" | awk '{print $2}')
    awk -v e="$e" -v b="$best" 'BEGIN{exit !(e+0<b+0)}' && { best=$e; }
    rss=$m
  done
  printf "  %-22s best=%ss  rss=%sKB\n" "$ev" "$best" "$rss"
  cmp -s /tmp/o.comb gmhs.comb && echo "    (byte-identical to gmhs.comb)" || echo "    (DIFFERS!)"
done
echo "=== done ==="
