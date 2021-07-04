drawille-tests: Drawille.icl Drawille.dcl main.icl
	clm -I /usr/lib64/clean/Platform/ -I /usr/lib64/clean/Gast/ -h 6000M main -o drawille-tests

clean:
	rm -f drawille-tests
	rm -rf "Clean System Files"
