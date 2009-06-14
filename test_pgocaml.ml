(* Example program using typesafe calls to PostgreSQL.
 *
 * PG'OCaml - type safe interface to PostgreSQL.
 * Copyright (C) 2005-2009 Richard Jones and other authors.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *)

open Printf

let () =
  let dbh = PGOCaml.connect () in

  PGSQL(dbh) "execute" "create temporary table employees (
     id serial not null primary key,
     name text not null,
     salary int4 not null,
     email text
  )";

  let insert name salary email =
    PGSQL(dbh) "insert into employees (name, salary, email)
                values ($name, $salary, $?email)"
  in
  insert "Ann" 10_000_l None;
  insert "Bob" 45_000_l None;
  insert "Jim" 20_000_l None;
  insert "Mary" 30_000_l (Some "mary@example.com");

  let rows = PGSQL(dbh) "select id, name, salary, email from employees" in
  List.iter (
    fun (id, name, salary, email) ->
      let email = match email with Some email -> email | None -> "-" in
      printf "%ld %S %ld %S\n" id name salary email
  ) rows;

  let ids = [ 1_l; 3_l ] in
  let rows = PGSQL(dbh) "select id, name, salary, email from employees
                          where id in $@ids" in
  List.iter (
    fun (id, name, salary, email) ->
      let email = match email with Some email -> email | None -> "-" in
      printf "%ld %S %ld %S\n" id name salary email
  ) rows;

  PGOCaml.close dbh
