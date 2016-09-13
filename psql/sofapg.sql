--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.4
-- Dumped by pg_dump version 9.5.4

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: sofapg; Type: SCHEMA; Schema: -; Owner: sofapg
--

CREATE SCHEMA sofapg;


ALTER SCHEMA sofapg OWNER TO sofapg;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: plv8; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plv8 WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plv8; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plv8 IS 'PL/JavaScript (v8) trusted procedural language';


--
-- Name: jsonb_extend; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS jsonb_extend WITH SCHEMA public;


--
-- Name: EXTENSION jsonb_extend; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION jsonb_extend IS 'jsonb_extend function';


SET search_path = sofapg, pg_catalog;

--
-- Name: map; Type: TYPE; Schema: sofapg; Owner: sofapg
--

CREATE TYPE map AS (
	view_id bigint,
	doc_id bigint,
	keys character varying[],
	val jsonb
);


ALTER TYPE map OWNER TO sofapg;

--
-- Name: resp; Type: TYPE; Schema: sofapg; Owner: sofapg
--

CREATE TYPE resp AS (
	condition boolean,
	details jsonb
);


ALTER TYPE resp OWNER TO sofapg;

--
-- Name: all_dbs(); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION all_dbs() RETURNS resp
    LANGUAGE sql
    AS $$
select resp(true, array_to_json(array(select title from dbs order by title))::jsonb);
$$;


ALTER FUNCTION sofapg.all_dbs() OWNER TO sofapg;

--
-- Name: all_docs(character varying, jsonb); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION all_docs(_db_name character varying, _opts jsonb) RETURNS resp
    LANGUAGE plpgsql
    AS $_$
declare
  q text;
  r json;
begin
  if _opts->>'include_docs' = 'true' then
  q =E'select key as id, S.*
      from (
        select *
        from (
          select key,
            (\'{"rev": "\' || id::varchar || \'-\' || md5(id::varchar) || \'"}\')::jsonb as value,
            val as doc
          from db_design
          where db_id=(select id from dbs where title=$1)
          order by key
        ) S1
        union all
        select *
        from (
          select key,
            (\'{"rev": "\' || id::varchar || \'-\' || md5(id::varchar) || \'"}\')::jsonb as value,
            val as doc
          from docs
          where db_id=(select id from dbs where title=$1)
          order by key
        ) S2
      ) S';
  else
  q =E'select key as id, S.*
      from (
        select *
        from (
          select key,
            (\'{"rev": "\' || id::varchar || \'-\' || md5(id::varchar) || \'"}\')::jsonb as value
          from db_design
          where db_id=(select id from dbs where title=$1)
          order by key
        ) S1
        union all
        select *
        from (
          select key,
            (\'{"rev": "\' || id::varchar || \'-\' || md5(id::varchar) || \'"}\')::jsonb as value
          from docs
          where db_id=(select id from dbs where title=$1)
          order by key
        ) S2
      ) S';
  end if;

  execute 'select array_to_json(array(
            select row_to_json(S.*) as r
            from('
              || q ||
            ') S
            ))' using _db_name into r;
  return resp(true, (
      select jsonb_extend(
        row_to_json(S.*)::jsonb,
        ('{"total_rows":' || json_array_length(rows) || '}')::jsonb
      )
      from (
        select 0 as offset,
        r as rows
      ) S
    )
  );
end
$_$;


ALTER FUNCTION sofapg.all_docs(_db_name character varying, _opts jsonb) OWNER TO sofapg;

--
-- Name: changes(character varying); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION changes(_db_name character varying) RETURNS resp
    LANGUAGE plpgsql
    AS $$
begin
  return resp(true, (
      select row_to_json(S.*)
      from (
        select array(
          select row_to_json(S.*) as result
          from (
            select key as id, seq(id)
            from db_design
            where db_id=(select id from dbs where title=_db_name)
            union all
            select key, seq(id)
            from docs
            where db_id=(select id from dbs where title=_db_name)
          ) S
        ) as results, seq() as last_seq
      ) S
    )::jsonb
  );

end
$$;


ALTER FUNCTION sofapg.changes(_db_name character varying) OWNER TO sofapg;

--
-- Name: create_error(character varying, character varying); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION create_error(_error character varying, _reason character varying) RETURNS jsonb
    LANGUAGE plpgsql IMMUTABLE
    AS $$
begin
  return (
    select row_to_json(R.*)
    from (
      select
        _error as error,
        _reason as reason
    ) R
  );
end
$$;


ALTER FUNCTION sofapg.create_error(_error character varying, _reason character varying) OWNER TO sofapg;

--
-- Name: db(character varying); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION db(_title character varying) RETURNS resp
    LANGUAGE plpgsql
    AS $$
begin
  perform from dbs where title=_title;
  if found then
    return (select resp(true,(
          select row_to_json(S.*)
          from (
            select
            title as db_name,
            seq() as update_seq,
            false as compact_running
            from dbs
            where title=_title
          ) S
        )::jsonb));
  end if;
  return (select resp(false,'{"error":"not_found","reason":"Database does not exist."}'::jsonb));
end
$$;


ALTER FUNCTION sofapg.db(_title character varying) OWNER TO sofapg;

--
-- Name: db_create(character varying); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION db_create(_title character varying) RETURNS resp
    LANGUAGE plpgsql
    AS $$
begin
  perform from dbs where title=_title;
  if found then
    return(
      select resp(false,
                  create_error('file_exists',
                              'The database could not be created, the file already exists')::jsonb));
  end if;
  insert into dbs(title) values (_title);
  return(
    select resp(true, (select row_to_json(R.*)::jsonb from (select true as ok) R))
  );
end
$$;


ALTER FUNCTION sofapg.db_create(_title character varying) OWNER TO sofapg;

--
-- Name: db_design(character varying, character varying); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION db_design(_db_name character varying, _key character varying) RETURNS resp
    LANGUAGE plpgsql
    AS $$
begin
  return resp(true, (
    select db_design.val from dbs
    join db_design on (dbs.id=db_design.db_id)
    where dbs.title=_db_name
    and db_design.key=_key
  ));
end
$$;


ALTER FUNCTION sofapg.db_design(_db_name character varying, _key character varying) OWNER TO sofapg;

--
-- Name: db_design_create(character varying, character varying, jsonb); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION db_design_create(_db_name character varying, _key character varying, _val jsonb) RETURNS resp
    LANGUAGE plpgsql
    AS $$
declare
  r resp;
begin
  r = verify_db_key(_db_name, _key, _val);
  if not r.condition then
    return r;
  end if;

  with
  design as (
    insert into db_design(db_id, key, val)
    select id,_key,jsonb_extend(_val, '{"views": null}'::jsonb)
    from dbs
    where title=_db_name
    returning id
  ),
  views as (
    insert into db_design_views(design_id, key)
    select id,key
    from design
    join (
      select jsonb_object_keys(_val->'views') as key
    ) S on true
    returning id,key
  ),
  funs as (
    insert into db_design_view_funs(view_id, key, val)
    select id,S.key,jsonb_object_field_text(jsonb_object_field(_val->'views', views.key), S.key)
    from views
    join lateral (
      select jsonb_object_keys(jsonb_object_field(_val->'views', views.key)) as key
    ) S on true
  )

  select true,'{"ok":true}'::jsonb into r;
  return r;
end
$$;


ALTER FUNCTION sofapg.db_design_create(_db_name character varying, _key character varying, _val jsonb) OWNER TO sofapg;

--
-- Name: db_view(character varying, character varying, character varying); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION db_view(_db_name character varying, _design_name character varying, _view_name character varying) RETURNS resp
    LANGUAGE plpgsql
    AS $$
begin
  return
  resp(true, (
      select jsonb_extend(
        row_to_json(S.*)::jsonb,
        ('{"total_rows":' || json_array_length(rows) || '}')::jsonb)
      from (
        select 0 as offset, array_to_json(array(
          with db as (select id from dbs where title=_db_name)
          ,doclist as (select id from docs where docs.db_id=(select id from db))
          ,vw as (
            select id from db_design_views
            where key=_view_name
            and design_id=(
              select id from db_design where key=_design_name
            )
          )
          select row_to_json(S.*) as r
          from doclist
          join lateral (
            select keys,val from map((select id from vw), doclist.id)
          ) S on true
        )) as rows
      ) S
    )
  );
end
$$;


ALTER FUNCTION sofapg.db_view(_db_name character varying, _design_name character varying, _view_name character varying) OWNER TO sofapg;

--
-- Name: doc(character varying, character varying); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION doc(_db_name character varying, _doc_id character varying) RETURNS resp
    LANGUAGE sql
    AS $$
  with db as (select id from dbs where title=_db_name),
       d  as (select val from docs where db_id=(select id from db) and key=_doc_id),
       c  as (select count(*) from d)
  select resp(true, val) from d where (select count=1 from c)
  union all
  select resp(false, create_error('not_found', 'missing')::jsonb)
  where (select count=0 from c);
$$;


ALTER FUNCTION sofapg.doc(_db_name character varying, _doc_id character varying) OWNER TO sofapg;

--
-- Name: doc_create(character varying, jsonb); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION doc_create(_db_name character varying, _doc jsonb) RETURNS resp
    LANGUAGE sql
    AS $$
select doc_create(_db_name, _doc->>'_id', _doc)
$$;


ALTER FUNCTION sofapg.doc_create(_db_name character varying, _doc jsonb) OWNER TO sofapg;

--
-- Name: doc_create(character varying, character varying, jsonb); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION doc_create(_db_name character varying, _key character varying, _val jsonb) RETURNS resp
    LANGUAGE plpgsql
    AS $$
declare
  r resp;
begin
  r = verify_db_key(_db_name, _key, _val);
  if not r.condition then
    return r;
  end if;
  insert into docs (db_id, key, val) select id, _key, _val from dbs where title=_db_name;
  return resp(true, '{"ok": true}'::jsonb);
end
$$;


ALTER FUNCTION sofapg.doc_create(_db_name character varying, _key character varying, _val jsonb) OWNER TO sofapg;

--
-- Name: map(bigint, bigint); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION map(_view_id bigint, _doc_id bigint) RETURNS TABLE(view_id bigint, doc_id bigint, keys jsonb, val jsonb)
    LANGUAGE plpgsql
    AS $$
begin
  return query
  select
    _view_id,_doc_id,value->'keys',value->'val'
  from jsonb_array_elements(
        map(
          (select T1.val from db_design_view_funs T1 where T1.view_id=_view_id and T1.key='map'),
          (
            select T2.val from docs T2
            where T2.id=_doc_id
            and db_id=(
              select db_id
              from db_design D
              join db_design_views DV
              on (D.id = DV.design_id)
              where DV.id=_view_id
            )
          )
        )
      )
  ;
end
$$;


ALTER FUNCTION sofapg.map(_view_id bigint, _doc_id bigint) OWNER TO sofapg;

--
-- Name: map(character varying, jsonb); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION map(_fun character varying, _doc jsonb) RETURNS jsonb
    LANGUAGE plv8
    AS $_$
var $doc = JSON.parse(_doc);
var fun = null;
var $fun = eval('fun = ' + _fun);
var $obj = [];
var emit = function(keys, val) {
  $obj.push({keys: keys, val: val});
};
$fun($doc);
return JSON.stringify($obj);
$_$;


ALTER FUNCTION sofapg.map(_fun character varying, _doc jsonb) OWNER TO sofapg;

--
-- Name: resp(boolean, jsonb); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION resp(cond boolean, data jsonb) RETURNS resp
    LANGUAGE plpgsql
    AS $$
declare
r resp;
begin
  r.condition = cond;
  r.details = data;
  return r;
end
$$;


ALTER FUNCTION sofapg.resp(cond boolean, data jsonb) OWNER TO sofapg;

--
-- Name: seq(bigint); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION seq(_seq bigint DEFAULT NULL::bigint) RETURNS jsonb
    LANGUAGE plpgsql
    AS $$
declare
  s bigint := _seq;
begin
  if s is null then
    s = (select last_value from rev);
  end if;
  return jsonb_extend(
    array_to_json(
      array(select s)
    )::jsonb,
    ((select row_to_json(options) from options where key='system_id')->'val')::jsonb
  );
end
$$;


ALTER FUNCTION sofapg.seq(_seq bigint) OWNER TO sofapg;

--
-- Name: verify_db_key(character varying, character varying, jsonb); Type: FUNCTION; Schema: sofapg; Owner: sofapg
--

CREATE FUNCTION verify_db_key(_db_name character varying, _key character varying, _val jsonb) RETURNS resp
    LANGUAGE plpgsql IMMUTABLE
    AS $$
begin
  if _key is null then
    return resp(false, create_error('no id', 'id is not defined')::jsonb);
  end if;
  if (select id from dbs where title=_db_name) is null then
    return resp(false, create_error('no db', 'db is not exist')::jsonb);
  end if;
  perform id from (select _val->>'_id' as id, _val->'_id' as jid) S
  where id is null or jid=('null'::jsonb) or id=_key;
  if not found then
    raise warning 'id: %', _val->>'_id';
    raise warning 'key: %', _key;
    return resp(false, create_error('wrong_id', 'wrong document id'));
  end if;
  if _key='null' then
    return resp(false, create_error('wrong_id', 'document id cannot be null'));
  end if;
  return resp(true, null);
end
$$;


ALTER FUNCTION sofapg.verify_db_key(_db_name character varying, _key character varying, _val jsonb) OWNER TO sofapg;

--
-- Name: rev; Type: SEQUENCE; Schema: sofapg; Owner: sofapg
--

CREATE SEQUENCE rev
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE rev OWNER TO sofapg;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: db_design; Type: TABLE; Schema: sofapg; Owner: sofapg
--

CREATE TABLE db_design (
    id bigint DEFAULT nextval('rev'::regclass) NOT NULL,
    db_id bigint,
    key character varying NOT NULL,
    val jsonb NOT NULL
);


ALTER TABLE db_design OWNER TO sofapg;

--
-- Name: db_design_view_funs; Type: TABLE; Schema: sofapg; Owner: sofapg
--

CREATE TABLE db_design_view_funs (
    id bigint DEFAULT nextval('rev'::regclass) NOT NULL,
    view_id bigint NOT NULL,
    key character varying NOT NULL,
    val character varying NOT NULL
);


ALTER TABLE db_design_view_funs OWNER TO sofapg;

--
-- Name: db_design_views; Type: TABLE; Schema: sofapg; Owner: sofapg
--

CREATE TABLE db_design_views (
    id bigint DEFAULT nextval('rev'::regclass) NOT NULL,
    design_id bigint,
    key character varying NOT NULL
);


ALTER TABLE db_design_views OWNER TO sofapg;

--
-- Name: dbs; Type: TABLE; Schema: sofapg; Owner: sofapg
--

CREATE TABLE dbs (
    id bigint DEFAULT nextval('rev'::regclass) NOT NULL,
    title character varying NOT NULL
);


ALTER TABLE dbs OWNER TO sofapg;

--
-- Name: docs; Type: TABLE; Schema: sofapg; Owner: sofapg
--

CREATE TABLE docs (
    id bigint DEFAULT nextval('rev'::regclass) NOT NULL,
    db_id bigint NOT NULL,
    key character varying NOT NULL,
    val jsonb NOT NULL
);


ALTER TABLE docs OWNER TO sofapg;

--
-- Name: options; Type: TABLE; Schema: sofapg; Owner: sofapg
--

CREATE TABLE options (
    key character varying NOT NULL,
    val character varying NOT NULL
);


ALTER TABLE options OWNER TO sofapg;

--
-- Name: view_doc; Type: TABLE; Schema: sofapg; Owner: sofapg
--

CREATE TABLE view_doc (
    id bigint DEFAULT nextval('rev'::regclass) NOT NULL,
    view_id bigint NOT NULL,
    doc_id bigint NOT NULL,
    keys character varying[] NOT NULL,
    val jsonb NOT NULL
);


ALTER TABLE view_doc OWNER TO sofapg;

--
-- Name: db_design_db_id_key_key; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY db_design
    ADD CONSTRAINT db_design_db_id_key_key UNIQUE (db_id, key);


--
-- Name: db_design_pkey; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY db_design
    ADD CONSTRAINT db_design_pkey PRIMARY KEY (id);


--
-- Name: db_design_view_funs_pkey; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY db_design_view_funs
    ADD CONSTRAINT db_design_view_funs_pkey PRIMARY KEY (id);


--
-- Name: db_design_view_funs_view_id_key_key; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY db_design_view_funs
    ADD CONSTRAINT db_design_view_funs_view_id_key_key UNIQUE (view_id, key);


--
-- Name: db_design_views_design_id_key_key; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY db_design_views
    ADD CONSTRAINT db_design_views_design_id_key_key UNIQUE (design_id, key);


--
-- Name: db_design_views_pkey; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY db_design_views
    ADD CONSTRAINT db_design_views_pkey PRIMARY KEY (id);


--
-- Name: dbs_pkey; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY dbs
    ADD CONSTRAINT dbs_pkey PRIMARY KEY (id);


--
-- Name: dbs_title_key; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY dbs
    ADD CONSTRAINT dbs_title_key UNIQUE (title);


--
-- Name: docs_db_key_key; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY docs
    ADD CONSTRAINT docs_db_key_key UNIQUE (db_id, key);


--
-- Name: docs_pkey; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY docs
    ADD CONSTRAINT docs_pkey PRIMARY KEY (id);


--
-- Name: options_pkey; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY options
    ADD CONSTRAINT options_pkey PRIMARY KEY (key);


--
-- Name: view_doc_pkey; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY view_doc
    ADD CONSTRAINT view_doc_pkey PRIMARY KEY (id);


--
-- Name: view_doc_view_id_doc_id_key; Type: CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY view_doc
    ADD CONSTRAINT view_doc_view_id_doc_id_key UNIQUE (view_id, doc_id);


--
-- Name: db_design_db_id_fkey; Type: FK CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY db_design
    ADD CONSTRAINT db_design_db_id_fkey FOREIGN KEY (db_id) REFERENCES dbs(id);


--
-- Name: db_design_view_funs_view_id_fkey; Type: FK CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY db_design_view_funs
    ADD CONSTRAINT db_design_view_funs_view_id_fkey FOREIGN KEY (view_id) REFERENCES db_design_views(id);


--
-- Name: db_design_views_design_id_fkey; Type: FK CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY db_design_views
    ADD CONSTRAINT db_design_views_design_id_fkey FOREIGN KEY (design_id) REFERENCES db_design(id);


--
-- Name: docs_db_fkey; Type: FK CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY docs
    ADD CONSTRAINT docs_db_fkey FOREIGN KEY (db_id) REFERENCES dbs(id);


--
-- Name: view_doc_doc_id_fkey; Type: FK CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY view_doc
    ADD CONSTRAINT view_doc_doc_id_fkey FOREIGN KEY (doc_id) REFERENCES docs(id);


--
-- Name: view_doc_view_id_fkey; Type: FK CONSTRAINT; Schema: sofapg; Owner: sofapg
--

ALTER TABLE ONLY view_doc
    ADD CONSTRAINT view_doc_view_id_fkey FOREIGN KEY (view_id) REFERENCES db_design_views(id);


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

