*Auto node placement
*Q-learning style node goal associations
 *There are general nodes, and then dsicovered nodes.
 *Discovered nodes are placed around important spots based on some event
  (you poll for things around you, find a tree. The node by you then gets more 'tree' goal weight)
 *Nodes you use that gets to a node that has a goal weight
  (while searching for that goal) will get it additional goal weight.
 *Goals can be followed negative or positive (trying to get away from a goal state, like a danger goal, or deciding to
   venture further from home instead of returning to it)
 *Goal weight is reduced by end-goal items.
 *Goals can be conditional (night danger vs overall danger)
 *Uses fitness (health/wealth) to judge the desire to explore.
 *Goal states can be represented by the desire to go from one internal state to another (learned behavior)
 *R-learning can maybe be used in the Behavior selection process. Can be useful if getting from one state to another requires
  other substates.
 *Behavior selection weighs need against ease (some things dont. so goals have a 'convenience'/direness factor).
 *Goal reduction also spreads (going from a higher goal weight value to a lower one lowers that first nodes weight).
  This helps unlearn things (going to a spot to find wood where there is no wood)
  while still preserving weights of alternate paths along the way.


Fixed Goals/Behaviors
*