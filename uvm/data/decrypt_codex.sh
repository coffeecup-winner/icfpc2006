pushd $(dirname $0) > /dev/null
pushd ../bin/Release > /dev/null
# decryption key copied from http://www.boundvariable.org/task.shtml
cat > decrypt.in <<EOF
(\b.bb)(\v.vv)06FHPVboundvarHRAk
p
EOF
./uvm data/codex.umz < decrypt.in > umix.umz.out
rm decrypt.in
tail +196c umix.umz.out > umix.umz
rm umix.umz.out
popd > /dev/null
mv ../bin/Release/umix.umz .
popd > /dev/null
