
tests : proprietary-test

# Tests that the output compiles, using
# the source code itself as the keyfile.
proprietary-test.sml : proprietary.sml proprietary.cm
	mlton proprietary.cm
	./proprietary -keyfile proprietary.sml > proprietary-test.sml

proprietary-test : proprietary-test.cm proprietary-test.sml proprietary-test-footer.sml
	mlton proprietary-test.cm
	./proprietary-test
