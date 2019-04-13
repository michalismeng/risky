
# appends the given text and data segments as %ICache and %DCache variables in the Spec.hs file

o_text="outputs/${1%.*}-text"
o_data="outputs/${1%.*}-data"

cat $o_text >> Spec.hs
cat $o_data >> Spec.hs