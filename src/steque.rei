/** The type of a deque containing elements of type ['a]. */
type t('a);

/** The [empty] deque. */

let empty: t('a);

/** [is_empty xs] returns [true] when the deque [xs] contains no elements, [false] if at least one. */

let is_empty: t('a) => bool;

/** [singleton x] returns a deque containing only a single element [x]. */

let singleton: 'a => t('a);

/** [cons x xs] adds an element [x] to the front of the deque [xs]. {b O(1)} */

let cons: ('a, t('a)) => t('a);

/** [uncons xs] pops the left-most element of the deque [xs]. {b O(1)}
    @return [None] if the deque is empty.
*/

let uncons: t('a) => option(('a, t('a)));

/** [snoc xs x] adds an element [x] to the back of the deque [xs]. {b O(1)} */

let snoc: (t('a), 'a) => t('a);

/** [append xs ys] concatenates the two deques [xs] and [ys]. {b O(1)} */

let append: (t('a), t('a)) => t('a);

/** [rev xs] reverses the order of the elements of [xs]. {b O(N)} */

let rev: t('a) => t('a);

/** [length xs] returns the number of elements contained in [xs]. {b O(N)} */

let length: t('a) => int;

/**/**/;

let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;

let fold_right: (('a, 'b) => 'b, t('a), 'b) => 'b;

let of_dequeue: Dequeue.t('a) => t('a);

let make: (int, 'a) => t('a);
