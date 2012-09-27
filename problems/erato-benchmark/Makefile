.SILENT: 

all: 
	$(MAKE) run 2>&1 | tee log
	$(MAKE) parse-log

run: go-bool go-int cxx-int cxx-bool c-int

N ?= 100000000

go-bool:
	echo $@
	6g erato-$@.go
	6l -o erato-$@ erato-$@.6
	time -p -f %e ./erato-$@ -N=$(N)

go-int: 
	echo $@
	6g erato-$@.go
	6l -o erato-$@ erato-$@.6
	time -p -f %e ./erato-$@ -N=$(N)

cxx-bool:
	echo $@
	g++ -o erato-$@ \
		-O3 -funroll-all-loops -fomit-frame-pointer \
		-DTYPE=bool erato-cxx.cpp 
	time -p -f %e ./erato-$@ $(N)

cxx-int:
	echo $@
	g++ -o erato-$@ \
		-O3 -funroll-all-loops -fomit-frame-pointer \
		-DTYPE=int erato-cxx.cpp 
	time -p -f %e ./erato-$@ $(N)

c-int:
	echo $@
	gcc -o erato-$@ -lm \
		-O3 -funroll-all-loops -fomit-frame-pointer erato-$@.c
	time -p -f %e ./erato-$@ $(N)

parse-log:
	printf "%10s %10s %8s %5s\n" "Languange" N Count Time ; \
	(echo "------------------------------------") ; \
	while read type ; do \
		read N && \
		read count && \
		read time && \
		printf "%10s %10s %8s %5s\n" $$type $$N $$count $$time ; \
	done < log
