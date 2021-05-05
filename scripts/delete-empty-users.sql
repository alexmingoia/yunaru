delete from users u
using authors a
left join followings fg on fg.user_id = a.user_id
left join entries e on e.author_url = a.url
where a.user_id = u.id and e.url is null and fg.user_id is null;
