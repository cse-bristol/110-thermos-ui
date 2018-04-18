CREATE TABLE buildings (
   id varchar(40) not null primary key,
   address varchar(512) not null,
   postcode varchar(10) not null,
   type varchar(16) not null,
   building_type varchar(16) not null,
   demand real not null,
   geometry geometry(POLYGON, 4326),
   connect_id varchar(40) not null
);
--;;
CREATE TABLE ways (
   id varchar(40) not null primary key,
   address varchar(512) not null,
   postcode varchar(10) not null,
   length real not null,
   start_id varchar(40) not null,
   end_id varchar(40) not null,
   geometry geometry(LINESTRING, 4326)
);
