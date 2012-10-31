all: unpack configure build release clean

NAME = lrzsz-0.12.20

unpack:
	gzip -dc $(NAME).tar.gz | tar xvf -

configure:
	(cd $(NAME) && ./configure)
	
build:
	(cd $(NAME) && make)
	
release:
	cp $(NAME)/src/lsz ./sz
	cp $(NAME)/src/lrz ./rz
	
clean:
	-(cd $(NAME) && make clean)
	-rm -rf $(NAME)/*
	-rmdir $(NAME)
