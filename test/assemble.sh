
# Assembles the given programs (at arguments $@) using rars and outputs the text and data in the ./outputs/ folder
# eg. ./assemble.sh riscv1.asm
# assembles file assembly/risv1.asm and outputs text and data at outputs/riscv1-{text,data}

for f in $@
do
    o_text="outputs/${f%.*}-text"
    o_data="outputs/${f%.*}-data"

    input=assembly/$f.asm

    > $o_data
    > $o_text
    ./rars.jar a nc dump .text BinaryText $o_text dump .data BinaryText $o_data $input

    head -16 $o_data | awk '{print "  0b"$0}' | awk '{print $0" :>"}' | sponge $o_data
    echo "${f%.*}_DCache = " | cat - $o_data | sponge $o_data
    echo "  Nil" >> $o_data

    cat $o_text | awk '{print "  0b"$0}' | awk '{print $0" :>"}' | sponge $o_text
    echo "${f%.*}_ICache = " | cat - $o_text| sponge $o_text
    echo -e "  Nil" >> $o_text
done