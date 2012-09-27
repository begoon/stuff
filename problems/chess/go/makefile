name = chess2

all: build run

build:
	6g $(name).go
	6l -o $(name) $(name).6

fmt:
	gofmt -w=true -spaces=true -tabindent=false -tabwidth=2 $(name).go

run: test-2x2 test-3x3 test-4x4 test-7x7

test-2x2 test-3x3 test-4x4 test-7x7:
	echo TEST: $@
	time -p ./$(name) <$@.txt >$@.actual
	diff -u $@.etalon $@.actual

pack:
	tar czvf chess2.tgz *go *etalon README makefile *txt

clean:
	rm -rf *.actual *.6 $(name)
	
