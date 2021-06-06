create table users (
  id uuid primary key,
  email text unique,
  password text,
  status text not null default 'unpaid',
  newsletter_id text unique,
  paid_until timestamptz,
  canceled_at timestamptz,
  stripe_customer_id text,
  created_at timestamptz not null
);

alter table users add constraint users_email_excl exclude using btree (lower(email) with =);

create table authors (
  url text not null unique,
  name text,
  image_url text,
  note text,
  imported_at timestamptz
);

alter table authors add constraint authors_url_excl exclude using btree (lower(url) with =);

create table magic_links (
  id uuid primary key,
  email text,
  expires_at timestamptz not null,
  action text not null,
  user_id uuid references users(id) on update cascade on delete cascade,
  sent_at timestamptz
);

create index magic_links_idx on magic_links (sent_at);

create table feeds (
  url text not null unique,
  author_url text not null references authors(url) on delete cascade on update cascade,
  name text,
  summary text,
  format text not null,
  import_error text,
  etag text,
  recent_entry_url text,
  updated_at timestamptz,
  imported_at timestamptz,
  created_at timestampz
);

alter table feeds add constraint feeds_url_excl exclude using btree (lower(url) with =);
create index on feeds(updated_at);
create index on feeds(imported_at);

create table featured (
  category text not null,
  feed_url text not null references feeds(url) on delete cascade on update cascade,
  summary text,
  primary key (category, feed_url)
);

create table followings (
  user_id uuid not null references users(id) on delete cascade,
  feed_url text not null references feeds(url) on delete cascade on update cascade,
  muted boolean not null,
  primary key (user_id, feed_url)
);

create unique index on followings (feed_url, user_id);

create table entries (
  url text not null,
  author_url text not null references authors(url) on delete cascade on update cascade,
  feed_url text not null references feeds(url) on delete cascade on update cascade,
  name text,
  content text,
  summary text,

  image_urls text,
  audio_urls text,
  video_urls text,

  reblogged_by text references authors(url) on update cascade,
  reblogged_at timestamptz,

  published_at timestamptz,
  updated_at timestamptz,
  imported_at timestamptz
);

create unique index entries_url_feed_url_key on entries (url, feed_url);
alter table entries add constraint entries_url_feed_url_key unique using index entries_url_feed_url_key;
alter table entries add constraint entries_url_feed_url_excl exclude using btree (lower(url) with =, lower(feed_url) with =);
create index on entries(published_at);
create index on entries(reblogged_at);
