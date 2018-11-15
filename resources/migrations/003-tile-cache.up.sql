CREATE TABLE tilecache (
       x int not null,
       y int not null,
       z int not null,
       bytes bytea not null,
       primary key (x, y, z)
)
--;;
CREATE TABLE tilemaxima (
       z int not null primary key,
       maximum real not null
)
