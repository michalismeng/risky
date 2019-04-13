# removes any installed text or data variables from the Spec.hs file

sed '/Beginning of auto generated compiled code/q' Spec.hs | sponge Spec.hs
