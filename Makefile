all: build

mondo:
	ros -S . -s mondo -e '(asdf:make :mondo/command)'

.PHONY: build
build: mondo

.PHONY: install
install:
	mv ./mondo /usr/local/bin

.PHONY: clean
clean:
	rm ./mondo
