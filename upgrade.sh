./stop.sh
git checkout -f -- .
git pull origin master
brunch build ./resources/static/brunch
make deps
cabal update
cabal install --disable-documentation
./uglify.sh
