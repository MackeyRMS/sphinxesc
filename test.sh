printf "%-30s%-30s%-30s%-10s\n" INPUT EXPECTED RESULT PASS
cat tests.txt | grep -v '^#' | grep -v '^ *$' | # skip blank lines
sed -e 's/ *#.*$//' -e 's/ *$//' |              # clear comments and whitespace
awk -F ' *:: *' -v OFS="\t" '{$1=$1; print}' |  # turn :: field separator into tab
while IFS=$'\t' read a b
do
  res=$(echo -n "$a" | sphinxesc | tr '\t' ' ' | sed -e 's/[ ]*$//' -e 's/^[ ]*//' )
  pass=$( if [ "$res" = "$b" ]; then echo PASS; else  echo '****FAIL***'; fi )

  printf "%-30s%-30s%-30s%-10s\n" "$a" "$b" "$res" "$pass"
done
