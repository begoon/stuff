target = queens

all:
	erlc $(target).erl
	erl -noshell -s $(target) solve -s init stop

classic:
	erlc $(target)_classic.erl
	erl -noshell -s $(target)_classic solve -s init stop

clean:
	-rm *.beam *.dump
