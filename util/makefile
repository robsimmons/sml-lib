default : test

test : *.cm *.sml
	mlton -const 'Exn.keepHistory true' test.cm
