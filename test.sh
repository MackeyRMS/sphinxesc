printf "%-30s%-30s%-30s%-10s\n" INPUT EXPECTED RESULT PASS
cat tests.txt | awk -F ' *:: *' -v OFS="\t" '{$1=$1; print}'| 
while IFS=$'\t' read a b
do
  res=$(echo -n "$a" | sphinxesc)
  pass=$( if [ "$res" = "$b" ]; then echo PASS; else  echo FAIL; fi )

  printf "%-30s%-30s%-30s%-10s\n" "$a" "$b" "$res" "$pass"
done