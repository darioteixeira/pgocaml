
let () = Printf.printf {|let default_port = %d
let default_user = %S
let default_password = %S
let default_unix_domain_socket_dir = %S
let default_comment_src_loc = %B|}
    5432
    "postgres"
    ""
    "/var/run/postgresql"
    false
