# PG'OCaml is a set of OCaml bindings for the PostgreSQL database.

Please note that this is not the first or only PGSQL bindings for
OCaml. Here are the others which you may want to consider:

* [postgresql-ocaml](https://mmottl.github.io/postgresql-ocaml/)
PostgreSQL-OCaml by Markus Mottl

* [ocamlodbc](http://home.gna.org/ocamlodbc/)
ODBC bindings by Max Guesdon which can be used to access PostgreSQL

* [ocamldbi](http://download.savannah.nongnu.org/releases/modcaml/) a Perl-like
DBI layer by the present author

PG'OCAML is different than the above bindings:

* It ISN'T just a wrapper around the C libpq library.  Instead it's a pure
OCaml library which talks the frontend/backend protocol directly with the
database.

* It has a camlp4 layer which lets you write SQL statements directly in your
code, TYPE SAFE at compile time, with TYPE INFERENCE into the SQL, and using
the full PostgreSQL SQL grammar (sub-selects, PG-specific SQL, etc.).  But
the flip side of this is that you need to have access to the database at
_compile_ time, so the type checking magic can be done; also if you change
the database schema, you will need to recompile your code to check it is
still correctly typed.

* (A minor point) - It requires PostgreSQL >= 7.4. The default interface
(`PGOCaml`) provided is synchronous. But it also supports any asynchronous
interface that implements the `PGOCaml_generic.THREAD` signature.

* It doesn't work with other databases, nor will it ever work with other
databases.

# Usage

PG'OCaml uses environment variables (or in-code parameters, which are [ill advised]
(https://hackernoon.com/how-to-use-environment-variables-keep-your-secret-keys-safe-secure-8b1a7877d69c))
to connect to your database both at compile-time and at runtime.

| Variable      | Default       | Additional information |
| ------------- | ------------- | ---------------------- |
| `PGHOST`      | | If this starts with a `/` or is unspecified, PG'OCaml assumes you're specifying a Unix domain socket. |
| `PGPORT`      | `5432`        | This is also the default PostgreSQL port. |
| `PGUSER`      | The username of the current user, or `postgres` if that can't be found. | |
| `PGDATABASE`  | falls back on `PGUSER` | |
| `PGPASSWORD`  | empty string  | |
| `PGPROFILING` | no profiling  | Indicates the file to write profiling information to. If it doesn't exist, don't profile |
| `COMMENT_SRC_LOC` | `no`      | If set to `yes`, `1`, or `on`, PG'OCaml will append a comment to each query indicating where it appears in the OCaml source code. This can be useful for logging. |

# Using the PPX

In addition to the camlp4 syntax extension, there is also a PPX
available for more recent versions of OCaml. The PPX aims to be more or less a
carbon copy of the camlp4 extension.

```ocaml
let () =
  let dbh = PGOCaml.connect () in
  let insert name salary =
    [%pgsql dbh "insert into employees (name, salary) VALUES ($name, $salary)"]
  in
  ignore(insert "Chris" 1_000.0);
  let get name =
    [%pgsql dbh "select salary from employees where name = $name"]
  in
  let () = [%pgsql dbh
      "execute"
      "CREATE TEMP TABLE IF NOT EXISTS employees (
        name TEXT PRIMARY KEY,
        salary FLOAT)"]
  in
  let name = "Chris" in
  let salary = get name
    |> List.hd
    |> function
        | Some(x) -> x
        | None -> raise(Failure "The database is probably broken.")
  in
  Printf.printf "%s's salary is %.02f\n" name salary;
  PGOCaml.close(dbh)
```

Prepared statements are checked in reverse order, so this will compile:

```ocaml
let () =
  let dbh = PGOCaml.connect () in
  let insert name salary =
    [%pgsql dbh "insert into employees (name, salary) VALUES ($name, $salary)"]
  in
  ignore(insert "Chris" 1_000.0);
  let get name =
    [%pgsql dbh "select salary from employees where name = $name"]
  in
  let () =
    [%pgsql dbh "execute" "
      CREATE TEMP TABLE IF NOT EXISTS employees (
        name TEXT PRIMARY KEY,
        salary FLOAT
      )"]
  in
  let name = "Chris" in
  let salary = get name
    |> List.hd
    |> function
        | Some(x) -> x
        | None -> raise(Failure "The database is probably broken.")
  in
  Printf.printf "%s's salary is %.02f\n" name salary;
  PGOCaml.close(dbh)
```

but the following will NOT:

```ocaml
let () =
  let dbh = PGOCaml.connect () in
  let () =
    [%pgsql dbh "execute" "
      CREATE TEMP TABLE IF NOT EXISTS employees (
        name TEXT PRIMARY KEY,
        salary FLOAT
      )"]
  in
  let insert name salary =
    [%pgsql dbh "insert into employees (name, salary) VALUES ($name, $salary)"]
  in
  ignore(insert "Chris" 1_000.0);
  let get name =
    [%pgsql dbh "select salary from employees where name = $name"]
  in
  let name = "Chris" in
  let salary = get name
    |> List.hd
    |> function
        | Some(x) -> x
        | None -> raise(Failure "The database is probably broken.")
  in
  Printf.printf "%s's salary is %.02f\n" name salary;
  PGOCaml.close(dbh)
```

----------------------------------------------------------------------

PG'OCaml (C) Copyright 2005-2009 Merjis Ltd, Richard W.M. Jones (rich@annexia.org)
and other authors (see CONTRIBUTORS.txt for details).

This software is distributed under the GNU LGPL with OCaml linking
exception.  Please see the file COPYING.LIB for full license.

----------------------------------------------------------------------

For an example, please see [tests](https://github.com/darioteixeira/pgocaml/blob/master/tests/test_pgocaml_highlevel.ml)
