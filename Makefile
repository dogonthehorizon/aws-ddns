# This Makefile attempts to follow the best-practice set out by Alexis King
# in "An opinionated guide to Haskell in 2018" where we build developer tooling
# as part of the project environment rather than globally. This ensures that
# tools like `ghcmod` are using the same version of GHC as our target runtime to
# get the most relevant results.
#
# https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
clean:
	@stack clean

# This should only need to be done once per developer machine.
setup: clean
	stack build --no-docker --copy-compiler-tool stylish-haskell hlint apply-refact

_HLINT=hlint {} \;
hlint:
	@find src/ -name "*.hs" -exec $(_HLINT)

_STYLISH=stylish-haskell -i {} \;
stylish-haskell:
	@find src/ -name "*.hs" -exec $(_STYLISH)

_BRITTANY=brittany --write-mode=inplace {} \;
brittany:
	@find src/ -name "*.hs" -exec $(_BRITTANY)

lint-all: stylish-haskell hlint brittany

test:
	stack test --test-arguments "--color always"

deploy: clean
	@stack build --copy-bins && ./deploy.sh
