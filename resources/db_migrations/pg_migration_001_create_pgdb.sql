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
