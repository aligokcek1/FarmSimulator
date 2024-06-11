:- dynamic (state/4).
:- dynamic(width/1).
:- dynamic(height/1).

object_props('wolf', 'W', animal).
object_props('chicken', 'H', animal).
object_props('cow', 'C', animal).
object_props('grass', 'G', food).
object_props('grain', 'F', food).
object_props('corn','M', food).
object_props('.', '.', '.').
object_props('.', '\n', '.').


can_eat(cow, grass).
can_eat(cow, grain).
can_eat(chicken, grain).
can_eat(chicken, corn).
can_eat(wolf, cow).
can_eat(wolf, chicken).

reproduction_ep(cow, 30).
reproduction_ep(chicken, 15).
reproduction_ep(wolf, 45).

energy_point(cow, 30).
energy_point(chicken, 15).
energy_point(grain, 10).
energy_point(grass, 20).
energy_point(corn, 5).

can_move(cow, move_up).
can_move(cow, move_down).
can_move(cow, move_left).
can_move(cow, move_right).
can_move(chicken, move_up_right).
can_move(chicken, move_up_left).
can_move(chicken, move_down_right).
can_move(chicken, move_down_left).
can_move(wolf, move_up).
can_move(wolf, move_down).
can_move(wolf, move_left).
can_move(wolf, move_right).
can_move(wolf, move_up_right).
can_move(wolf, move_up_left).
can_move(wolf, move_down_right).
can_move(wolf, move_down_left).

can_make(cow, idle).
can_make(cow, eat).
can_make(chicken, idle).
can_make(chicken, eat).
can_make(wolf, idle).
can_make(wolf, eat).

value(cow, 500).
value(chicken, 100).
value(corn, 10).
value(grain, 30).
value(grass, 20).