
default : test.exe

test.exe : ../../util/*.sml *.sml test.cm
	mlton -output $@ -const 'Exn.keepHistory true' test.cm
