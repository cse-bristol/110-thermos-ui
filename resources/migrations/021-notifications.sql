ALTER TABLE users ADD COLUMN job_emails boolean NOT NULL DEFAULT false
--;;
CREATE TABLE push_subscriptions (
   endpoint text not null primary key,
   user_id text not null references users(id) on delete cascade,
   p256dh text not null,
   auth text not null,
   created timestamp without time zone default (now() at time zone 'utc')
)
--;;
CREATE TABLE vapid_keys (
   id integer not null primary key default 1 check (id = 1),
   public_key text not null,
   private_key text not null
)
