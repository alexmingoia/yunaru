delete from users u
using users u1
left join followings fg on fg.user_id = u1.id
where u1.status = 'unpaid' and u1.id = u.id and fg.user_id is null;
