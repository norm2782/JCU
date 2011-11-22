CREATE TABLE rules (
  rid serial PRIMARY KEY,
  uid text,
  rule_order integer,
  rule text)

CREATE TABLE users (
    uid serial PRIMARY KEY,
    last_login_at text,
    meta text,
    roles text,
    updated_at text,
    created_at text,
    last_login_ip text,
    current_login_ip text,
    current_login_at text,
    locked_out_until text,
    failed_login_count integer,
    login_count integer,
    suspended_at text,
    activated_at text,
    remember_token text,
    email text,
    password text
)