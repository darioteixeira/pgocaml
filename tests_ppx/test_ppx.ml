let init_dbh dbh =
  [%pgsql dbh "execute" "create temporary table employees
(
id serial not null primary key,
name text not null,
salary int4 not null,
email text
)"]

let employee_exists dbh ?email n =
  [%pgsql dbh "SELECT EXISTS (SELECT 1 FROM employees WHERE name = $n AND email = $?email AND email = $email)"]

let () =
  let dbh = PGOCaml.connect () in

  init_dbh dbh;

  let insert name salary email = [%pgsql dbh "insert into employees (name, salary, email) values ($name, $salary, $?email)"] in
  insert "Ann" 10_000_l None;
  insert "Bob" 45_000_l None;
  insert "Jim" 20_000_l None;
  insert "Mary" 30_000_l (Some "mary@example.com");

  let rows = [%pgsql dbh "select id, name, salary, email from employees"] in
  List.iter
    begin
      fun (id, name, salary, email) ->
        let email = match email with Some email -> email | None -> "-" in
        Printf.printf "%ld %S %ld %S\n" id name salary email
    end rows;

  let ids = [ 1_l; 3_l ] in
  let rows = [%pgsql dbh "select id, name, salary, email from employees where id in $@ids"] in
  List.iter
    begin
      fun (id, name, salary, email) ->
        let email = match email with Some email -> email | None -> "-" in
        Printf.printf "%ld %S %ld %S\n" id name salary email
    end rows;

  PGOCaml.close dbh
