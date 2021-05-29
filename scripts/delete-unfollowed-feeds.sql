delete from feeds
using feeds fd
left join (
  select count(*) as num, feed_url
  from followings
  group by feed_url
) fg on fg.feed_url = fd.url
where fg.num is null and feeds.url = fd.url;
