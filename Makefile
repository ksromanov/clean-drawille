drawille-example: Drawille.icl
	clm -I /usr/lib64/clean/Platform/ -I /usr/lib64/clean/Gast/ -h 6000M Drawille -o drawille-example

clean:
	rm -f drawille-example
	rm -rf "Clean System Files"
