pushd $(dirname $0) > /dev/null
curl http://www.boundvariable.org/codex.umz -o codex.umz
curl http://www.boundvariable.org/sandmark.umz -o sandmark.umz
echo "Build UVM in Release then run decrypt_codex.sh to get UMIX" | tee umix.umz
popd > /dev/null
