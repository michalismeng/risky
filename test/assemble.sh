
# Assembles the given programs (at arguments $@) using rars and outputs the text and data in the ./outputs/ folder
# eg. ./assemble.sh riscv1
# assembles file assembly/risv1.asm and outputs text and data as Haskell variables in Out/Riscv1.hs


preamble="{-# LANGUAGE NoImplicitPrelude, DataKinds, BinaryLiterals #-}"

for f in $@
do
    input=assembly/$f.asm

    if ! [ -f "$input" ]; then
        echo "File '$input' does not exist"
        exit
    fi

    o_text=$(mktemp)
    o_data=$(mktemp)

    java -jar ./rars.jar a nc dump .text BinaryText $o_text dump .data BinaryText $o_data $input

    head -16 $o_data | awk '{print "  (0b"$0" :: BitVector 32) :>"}' | sponge $o_data
    echo "${f%.*}_DCache = " | cat - $o_data | sponge $o_data
    echo "  Nil" >> $o_data

    cat $o_text | awk '{print "  (0b"$0" :: BitVector 32) :>"}' | sponge $o_text
    echo "${f%.*}_ICache = " | cat - $o_text| sponge $o_text
    echo -e "  Nil" >> $o_text

    o_haskell="Out/${f^}.hs"

    echo -e "$preamble\n" > $o_haskell
    echo -e "module Out.${f^} where\n\nimport Clash.Prelude\n" >> $o_haskell
    cat $o_text $o_data >> $o_haskell

    rm -f $o_text $o_data
done