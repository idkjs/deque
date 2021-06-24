type t('a);
let of_deque: Deque.t('a) => option(t('a));
let to_deque: t('a) => Deque.t('a);
let go_left: t('a) => option(t('a));
let go_right: t('a) => option(t('a));
let focus: t('a) => 'a;
