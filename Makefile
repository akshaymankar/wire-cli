build:
	cabal build

test-unit:
	cabal test --test-show-details=always unit

run-gui:
	cabal run wire-gui
