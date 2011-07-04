
default : test.exe

metautil.exe : metautil.cm metautil.sml
	mlton -output $@ metautil.cm

# also generates sig
cells.sml : metautil.exe
	./metautil.exe

test.exe : test.cm *.sml bdd.cm cells.sml
	mlton -output $@ test.cm
