package main

import (
	"database/sql"
	"html/template"
	"log"
	"net/http"
	"net/url"
	"os"
	"runtime"
	"sort"

	_ "github.com/mattn/go-sqlite3"

	"github.com/gorilla/mux"
)

var (
	tpls = template.Must(template.ParseGlob("templates/*.html"))

	dbPath = defaultStr(os.Getenv("DB_PATH"), ":memory:")
	//inSandstorm = os.Getenv("SANDSTORM") == "1"
)

const (
	Cleared SortCol = iota
	Up
	Down
)

type SortCol int

func (s SortCol) ToggleSym() string {
	switch s {
	case Cleared:
		return ""
	case Up:
		return "\u2191"
	case Down:
		return "\u2193"
	default:
		panic(s)
	}
}

func (s SortCol) ToggleName() string {
	switch s {
	case Cleared, Up:
		return "asc"
	case Down:
		return "desc"
	default:
		panic(s)
	}
}

func (s SortCol) Invert() SortCol {
	if s == Up {
		return Down
	} else {
		return Up
	}
}

type TODO struct {
	Id    int
	Done  bool
	Descr string
}

type Page struct {
	DoneSort, DescrSort SortCol
	Todos               []TODO
}

func (p Page) SortCol() string {
	if p.DoneSort == Cleared {
		return "descr"
	} else if p.DescrSort == Cleared {
		return ""
	} else {
		return "done"
	}
}

func (p Page) SortDir() string {
	if p.DoneSort == Cleared {
		return p.DescrSort.ToggleName()
	} else {
		return p.DoneSort.ToggleName()
	}
}

func main() {
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
				DoneSort:  Cleared,
				DescrSort: Cleared,
			}
			todos := []TODO{}
			less := func(i, j int) bool {
				return todos[i].Id < todos[j].Id
			}

			switch req.FormValue("sort_col") {
			case "done":
				switch req.FormValue("sort_dir") {
				case "asc":
					page.DoneSort = Up
					less = func(i, j int) bool {
						return !todos[i].Done && todos[j].Done
					}
				case "desc":
					page.DoneSort = Down
					less = func(i, j int) bool {
						return todos[i].Done && !todos[j].Done
					}
				}
			case "descr":
				switch req.FormValue("sort_dir") {
				case "asc":
					page.DescrSort = Up
					less = func(i, j int) bool {
						return todos[i].Descr < todos[j].Descr
					}
				case "desc":
					page.DescrSort = Down
					less = func(i, j int) bool {
						return todos[i].Descr > todos[j].Descr
					}
				}
			}

			rows, err := db.Query(`
				SELECT id, done, descr
				FROM "todos"
			`)
			if chkSrvErr(w, err) {
				return
			}
			defer rows.Close()
			for rows.Next() {
				var next TODO
				if chkSrvErr(w, rows.Scan(
					&next.Id,
					&next.Done,
					&next.Descr,
				)) {
					return
				}
				todos = append(todos, next)
			}
			rows.Close()
			sort.Slice(todos, less)
			page.Todos = todos
			tpls.ExecuteTemplate(w, "index.html", page)
		})

	r.Methods("POST").Path("/todos/new").
		HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			_, err := db.Exec(`
				INSERT INTO todos(done, descr)
				VALUES ('false', ?)
			`, req.FormValue("descr"))
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
			if chkSrvErr(w, err) {
				return
			}
			doRedirect(w, req)
		})

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
