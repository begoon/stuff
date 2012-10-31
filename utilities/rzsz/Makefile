all: unpack build release clean

NAME = rzsz-3.49

unpack:
	gzip -dc $(NAME).tar.gz | tar xvf -
	
build:
	(cd $(NAME) && make posix)
	
release:
	cp $(NAME)/sz ./
	strip sz
	cp $(NAME)/rz ./
	strip rz	
	
clean:
	-(cd $(NAME) && make clean)
	-rm -rf $(NAME)/*
	-rmdir $(NAME)
