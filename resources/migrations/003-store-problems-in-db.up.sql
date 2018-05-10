CREATE TABLE problems (
    id serial not null primary key,
    org varchar(512) not null,
    name varchar(512) not null,
    content text not null,
    created timestamp without time zone default (now() at time zone 'utc'),
    format int not null default 0,
    has_run boolean not null default false
);
--;;
CREATE TABLE jobs (
    id serial not null primary key,
    args text not null,
    state smallint not null,
    queue text not null
);
--;;
CREATE INDEX job_state_idx on jobs (state, id);
--;;
CREATE INDEX job_state_queue_idx on jobs (state,queue,id);
