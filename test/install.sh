
# appends the given text and data segments as %ICache and %DCache variables in the Spec.hs file

for f in $@
do
    o_text="outputs/${f%.*}-text"
    o_data="outputs/${f%.*}-data"

    cat $o_text >> Spec.hs
    cat $o_data >> Spec.hs
done