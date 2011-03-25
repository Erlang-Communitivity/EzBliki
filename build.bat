rm -rf ebin
mkdir ebin
pushd ebin
mkdir -p bliki/app/atom
mkdir -p bliki/behaviour
mkdir -p bliki/lib/appmod
mkdir -p bliki/lib/routes
mkdir -p bliki/lib/templates
mkdir -p bliki/lib/openid
popd
echo in dir: `pwd`
cp resources/*.app ebin/
pushd src
erl -noshell -s make all -s init stop
popd

