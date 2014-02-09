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
*Dead-End - Exploration goal that results

Properties-
*A node property shows up in one of 3 ways:
  *A value on the node that spreads when a connection to it is satisfied
  *A value on the connecting leading from the node elsewhere


Exploration-
*All nodes have a familiarity property to them, moving from this node spreads its property
*Going to the most unfamiliar places
*Enforce a 'craving' for new Wps, if exploring and getting nowere, increase explore



2-7-2014  -- Pathing milestone! Capability to handle multiple goals.
Most of the above has been implemented.




Waypoint/Goal Refactor
*Real state -> action association.
*Waypoints hold connections to non-terrestrial states
  *Waypoints are terrestrial goal states whose action is simply the move to that location.
  *Terrestrial waypoints may link to non-terrestrial goals
    *eg Waypoint may connect to a 'mining' state
    *Any action state always has a link back to the last waypoint
  *Non-terrestrial goals allow concepts to be built independant of location

*Include random actions within the policy selection process

Block Manipulation
*Use WorldView to mock up potential changes and simulate from there.
  *Test for potential breaking of paths.
  *Test if changes expose additional paths
  *Test possibles changes to see if it increases the maxQ (with respect to desire) of last waypoint.
  *
*When seeing a player, signal a 'player' reward that is a detractor when doing block manipulation.
*When manipulating blocks, signal a reward for 'own'
*When seeing strange blocks away from 'own' or 'home', reward 'player'
*Manipulation weight = own - home - player

Path Coherence Q-learning (unnecessary, use static logic)
*Learn to follow a path with a simple reward system
  *Current state is the current block we are in.
    //*Discrete state(isCentered: Boolean,
    //    solid for: neighbors in a 3x3x3 area), goingTo: Which neighbor)
    *Corner - blocked
    *Corner - open
    *Jump - Corner - block
  *Actions
    *Nudge - nudge to last target (center)
    *Jump
    *Move - 1 of 4 directions
  *An action may not result in a state change, in which case, repeat.

