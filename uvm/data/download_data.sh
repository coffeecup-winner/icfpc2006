pushd $(dirname $0) > /dev/null
curl http://www.boundvariable.org/codex.umz -o codex.umz
curl http://www.boundvariable.org/sandmark.umz -o sandmark.umz
popd > /dev/null
