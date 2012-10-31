.SILENT: 

help:
	@echo build      - build everything
	@echo test       - perform tests
	@echo clean      - clean-un

test: build test-cpp test-java test-cs test-php loopback

PACKER ?= EMPTY
UNPACKER ?= EMPTY

loopback = loopback
testsource = $(ComSpec)

silent = -s --no-print-directory

SMAKE = $(MAKE) $(silent)

loopback: build
	copy $(testsource) $(loopback).original >nul
	$(SMAKE) test-loopback PACKER=php UNPACKER=php
	$(SMAKE) test-loopback PACKER=cs UNPACKER=cs
	$(SMAKE) test-loopback PACKER=cpp UNPACKER=cpp
	$(SMAKE) test-loopback PACKER=java UNPACKER=java
	$(SMAKE) loopback-pair S1=php S2=cs
	$(SMAKE) loopback-pair S1=php S2=cpp
	$(SMAKE) loopback-pair S1=php S2=java
	$(SMAKE) loopback-pair S1=cs S2=cpp
	$(SMAKE) loopback-pair S1=cs S2=java
	$(SMAKE) loopback-pair S1=cpp S2=java

loopback-pair:
	$(SMAKE) test-loopback PACKER=$(S1) UNPACKER=$(S2)
	$(SMAKE) test-loopback PACKER=$(S2) UNPACKER=$(S1)

test-loopback:
	echo $(PACKER) -^> $(UNPACKER)
	$(SMAKE) -C $(PACKER) deflate ifile=..\$(loopback).original ofile=..\$(loopback).packed
	$(SMAKE) -C $(UNPACKER) inflate ifile=..\$(loopback).packed ofile=..\$(loopback).unpacked
	fc /b $(loopback).original $(loopback).unpacked >nul || echo FAILED!
	del $(loopback).packed $(loopback).unpacked
	
test-cpp:
	$(MAKE) -C cpp test
	
test-java:
	$(MAKE) -C java test
	
test-cs:
	$(MAKE) -C cs test
	
test-php:
	$(MAKE) -C php test

build:
	$(MAKE) -C cpp $@
	$(MAKE) -C java $@
	$(MAKE) -C cs $@
	$(MAKE) -C php $@

clean: 
	$(MAKE) -C cpp $@
	$(MAKE) -C java $@
	$(MAKE) -C cs $@
	$(MAKE) -C php $@
	-@del $(loopback).*

