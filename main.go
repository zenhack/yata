package main

import (
	"context"
	"database/sql"
	"html/template"
	"log"
	"net/http"
	"net/url"
	"os"
	"runtime"
	"strconv"
	"sync"

	_ "github.com/mattn/go-sqlite3"

	"github.com/gorilla/mux"
)

var (
	tpls = template.Must(template.ParseGlob("templates/*.html"))

	dbPath = defaultStr(os.Getenv("DB_PATH"), ":memory:")
)

type SortDir string

const (
	Asc  SortDir = "asc"
	Desc SortDir = "desc"
)

type SortInfo struct {
	Col string
	Dir SortDir
}

type NotifyState struct {
	sync.Mutex
	ctx     context.Context
	cancel  context.CancelFunc
	version int64
}

func (d SortDir) Invert() SortDir {
	switch d {
	case Asc:
		return Desc
	case Desc:
		return Asc
	default:
		panic("Invalid SortDir: " + string(d))
	}
}

func (s *NotifyState) Version() int64 {
	s.Lock()
	defer s.Unlock()
	return s.version
}

func getTodos(db *sql.DB, columnName string, dir SortDir) ([]TODO, error) {
	// XXX: I am dissatisfied with this. Gluing strings together to
	// build queries is a big no-no, but Query won't do substitution for
	// column names and asc/desc. It's either this or a bunch of
	// repetative code. Some kind of query builder library would be nice
	// here.

	if dir != Asc && dir != Desc {
		panic("invalid SortDir: " + string(dir))
	}
	if columnName != "id" && columnName != "done" && columnName != "descr" {
		panic("invalid columnName: " + columnName)
	}

	rows, err := db.Query(`
		SELECT id, done, descr
		FROM "todos"
		ORDER BY `+columnName+` COLLATE NOCASE `+string(dir),
		columnName)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	todos := []TODO{}
	for rows.Next() {
		var next TODO
		err := rows.Scan(&next.Id, &next.Done, &next.Descr)
		if err != nil {
			return nil, err
		}
		todos = append(todos, next)
	}
	return todos, nil
}

func NewNotifyState() *NotifyState {
	ctx, cancel := context.WithCancel(context.Background())
	return &NotifyState{
		ctx:    ctx,
		cancel: cancel,
	}
}

func (s *NotifyState) Wait(version int64) <-chan struct{} {
	s.Lock()
	defer s.Unlock()
	if s.version >= version {
		ch := make(chan struct{}, 1)
		ch <- struct{}{}
		return ch
	} else {
		return s.ctx.Done()
	}
}

func (s *NotifyState) Notify() {
	s.Lock()
	defer s.Unlock()
	s.version++
	s.cancel()
	s.ctx, s.cancel = context.WithCancel(context.Background())
}

func (s SortDir) ArrowChar() string {
	switch s {
	case Asc:
		return "\u2191"
	case Desc:
		return "\u2193"
	default:
		panic("Invalid sort dir:" + string(s))
	}
}

type TODO struct {
	Id    int
	Done  bool
	Descr string
}

type Page struct {
	Version int64
	SortInfo
	Todos []TODO
}

func RequestSortInfo(req *http.Request) SortInfo {
	info := SortInfo{
		Col: req.FormValue("sort_col"),
		Dir: SortDir(req.FormValue("sort_dir")),
	}
	switch info.Col {
	case "id", "done", "descr":
	default:
		info.Col = "id"
	}
	switch info.Dir {
	case Asc, Desc:
	default:
		info.Dir = Asc
	}
	return info
}

func main() {
	nstate := NewNotifyState()

	db, err := sql.Open("sqlite3", dbPath)
	chkfatal(err)

	_, err = db.Exec(`CREATE TABLE IF NOT EXISTS todos (
		id INTEGER PRIMARY KEY,
		done BOOL NOT NULL,
		descr VARCHAR NOT NULL
	)`)
	chkfatal(err)

	r := mux.NewRouter()
	r.Methods("GET").Path("/").
		HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			page := Page{
				Version:  nstate.Version() + 1,
				SortInfo: RequestSortInfo(req),
			}
			todos, err := getTodos(db, page.SortInfo.Col, page.SortInfo.Dir)
			if chkSrvErr(w, err) {
				return
			}
			page.Todos = todos
			tpls.ExecuteTemplate(w, "index.html", page)
		})

	r.Methods("POST").Path("/todos/new").
		HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			_, err := db.Exec(`
				INSERT INTO todos(done, descr)
				VALUES (0, ?)
			`, req.FormValue("descr"))
			nstate.Notify()
			if chkSrvErr(w, err) {
				return
			}
			doRedirect(w, req)
		})

	r.Methods("POST").Path("/todos/delete/{id:[0-9]+}").
		HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			_, err := db.Exec(`
				DELETE FROM todos
				WHERE id = ?
			`, mux.Vars(req)["id"])
			nstate.Notify()
			if chkSrvErr(w, err) {
				return
			}
			doRedirect(w, req)
		})

	r.Methods("POST").Path("/todos/{id:[0-9]+}/done").
		HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			_, err := db.Exec(`
				UPDATE todos
				SET done = ?
				WHERE id = ?`,
				isYes(req.FormValue("done")),
				mux.Vars(req)["id"])
			nstate.Notify()
			if chkSrvErr(w, err) {
				return
			}
			doRedirect(w, req)
		})

	r.Methods("GET").Path("/version/{version:[0-9]+}").
		HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			i, err := strconv.ParseInt(
				mux.Vars(req)["version"],
				10,
				64,
			)
			if err != nil {
				w.WriteHeader(http.StatusBadRequest)
				w.Write([]byte(err.Error()))
				return
			}
			<-nstate.Wait(i)
		})
	r.Methods("GET").PathPrefix("/static/").Handler(http.FileServer(http.Dir("")))

	panic(http.ListenAndServe(":8080", r))
}

func isYes(s string) bool {
	return s == "yes" || s == "on"
}

func defaultStr(val, def string) string {
	if val == "" {
		return def
	}
	return val
}

func chkfatal(err error) {
	if err != nil {
		panic(err)
	}
}

func chkSrvErr(w http.ResponseWriter, err error) bool {
	if err == nil {
		return false
	}
	_, file, line, ok := runtime.Caller(1)
	if ok {
		log.Printf("chkSrvErr: %q line %d: %v", file, line, err)
	} else {
		log.Println("chkSrvErr (unknown source location):", err)
	}
	w.WriteHeader(http.StatusInternalServerError)
	return true
}

func doRedirect(w http.ResponseWriter, req *http.Request) {
	var path string
	col := req.FormValue("sort_col")
	dir := req.FormValue("sort_dir")
	if col == "" || dir == "" {
		path = "/"
	} else {
		path = "/?" + url.Values{
			"sort_col": {col},
			"sort_dir": {dir},
		}.Encode()
	}
	http.Redirect(w, req, path, http.StatusSeeOther)
}
