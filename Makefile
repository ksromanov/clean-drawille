drawille-example: Drawille.icl Drawille.dcl main.icl
	clm -I /usr/lib64/clean/Platform/ -I /usr/lib64/clean/Gast/ -h 6000M main -o drawille-example

clean:
	rm -f drawille-example
	rm -rf "Clean System Files"
