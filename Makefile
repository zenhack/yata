
icon_sizes := 24 48 128 256 150 300 600

all: app
srv: app
	DB_PATH=./db.sqlite3 ./app
dev: app
	spk dev
app: $(wildcard *.go)
	go build -v -i -o $@
	strip $@
clean:
	rm -f app
make-icons/make-icons: make-icons/main.go
	cd make-icons && go build
icons: make-icons/make-icons checkbox.png
	for size in $(icon_sizes); do \
		./make-icons/make-icons -size $$size < checkbox.png > icons/$$size.png ; \
	done
pack: app icons sandstorm-pkgdef.capnp
	spk pack yata.spk

.PHONY: all dev clean icons pack
