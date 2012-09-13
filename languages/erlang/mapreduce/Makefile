target = find_variables

all:
	erlc $(target).erl
	erlc phofs.erl
	erl -noshell -s $(target) main -s init stop

clean:
	-rm *.beam *.dump
