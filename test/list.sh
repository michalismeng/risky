
# lists available compiled programs
echo "Currently compiled modules"
for f in Out/*.hs
do
    name="$(basename $f)"
    module="Out.${name%.*}"
    echo "$module"
done