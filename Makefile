
test:
	nix shell nixpkgs#idris2 -c idris2 --build tests/tests.ipkg
	./tests/build/exec/penultimate-tests

build-browser:
	cd browser && nix shell nixpkgs#idris2 -c idris2 --build browser.ipkg
