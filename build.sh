rm -rf ebin
mkdir ebin
pushd ebin
mkdir -p bliki/app/atom
mkdir -p bliki/behaviour
mkdir -p bliki/appmod
mkdir -p bliki/model
mkdir -p bliki/openid
popd
cp resources/*.app ebin/
pushd src
erl -noshell -s make all -s init stop
popd
