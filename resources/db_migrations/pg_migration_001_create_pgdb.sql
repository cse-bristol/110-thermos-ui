CREATE EXTENSION postgis;

CREATE TABLE public.buildings
(
    type integer NOT NULL,
    "isSupply" boolean NOT NULL,
    "isDemand" boolean NOT NULL,
    "isPath" boolean NOT NULL,
    id integer NOT NULL,
    geometry geometry NOT NULL,
    CONSTRAINT buildings_pkey PRIMARY KEY (id)
)
WITH (
    OIDS = FALSE
)

CREATE TABLE public.connections
(
    id character varying COLLATE pg_catalog."default" NOT NULL,
    geom geometry(LineString,4326),
    distname character varying COLLATE pg_catalog."default",
    roadnumber character varying COLLATE pg_catalog."default",
    classification character varying COLLATE pg_catalog."default",
    "demand-id" character varying COLLATE pg_catalog."default",
    nodes character varying COLLATE pg_catalog."default",
    CONSTRAINT connections_pkey PRIMARY KEY (id)
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE public.connections
    OWNER to postgres;

-- Index: sidx_connections_geom

-- DROP INDEX public.sidx_connections_geom;

CREATE INDEX sidx_connections_geom
    ON public.connections USING gist
    (geom)
    TABLESPACE pg_default;
