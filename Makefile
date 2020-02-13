
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

.PHONY: all dev clean
