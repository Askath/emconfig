.PHONY: clean

clean: 
	git clean -dfX
	git clean -f

install:
	./install.sh
