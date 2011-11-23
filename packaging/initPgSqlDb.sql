--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: rules; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE rules (
    rid integer NOT NULL,
    uid integer,
    rule_order integer,
    rule text
);


ALTER TABLE public.rules OWNER TO postgres;

--
-- Name: rules_rid_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE rules_rid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.rules_rid_seq OWNER TO postgres;

--
-- Name: rules_rid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE rules_rid_seq OWNED BY rules.rid;


--
-- Name: users; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE users (
    meta text,
    roles text,
    failed_login_count integer,
    login_count integer,
    current_login_ip text,
    uid integer NOT NULL,
    email text,
    password text,
    remember_token text,
    last_login_ip text,
    updated_at timestamp with time zone,
    current_login_at timestamp with time zone,
    suspended_at timestamp with time zone,
    activated_at timestamp with time zone,
    created_at timestamp with time zone,
    last_login_at timestamp with time zone,
    locked_out_until timestamp with time zone
);


ALTER TABLE public.users OWNER TO postgres;

--
-- Name: users_uid_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE users_uid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_uid_seq OWNER TO postgres;

--
-- Name: users_uid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE users_uid_seq OWNED BY users.uid;


--
-- Name: rid; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE rules ALTER COLUMN rid SET DEFAULT nextval('rules_rid_seq'::regclass);


--
-- Name: uid; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE users ALTER COLUMN uid SET DEFAULT nextval('users_uid_seq'::regclass);


--
-- Name: rules_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY rules
    ADD CONSTRAINT rules_pkey PRIMARY KEY (rid);


--
-- Name: uid; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT uid PRIMARY KEY (uid);


--
-- Name: fki_uid; Type: INDEX; Schema: public; Owner: postgres; Tablespace: 
--

CREATE INDEX fki_uid ON rules USING btree (uid);


--
-- Name: rules_uid; Type: INDEX; Schema: public; Owner: postgres; Tablespace: 
--

CREATE INDEX rules_uid ON rules USING btree (uid);


--
-- Name: fk_uid; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY rules
    ADD CONSTRAINT fk_uid FOREIGN KEY (uid) REFERENCES users(uid);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

