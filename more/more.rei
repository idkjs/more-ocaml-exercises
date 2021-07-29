/* Tail-recursive lists */
module List: {include (module type of List);};

/* Utilities */
module Util: {
  let from: (int, int) => list(int);
  let take: (list('a), int) => list('a);
  let drop: (list('a), int) => list('a);
};
