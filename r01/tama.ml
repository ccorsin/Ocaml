class tama (health, hygiene, energy, happyness) =

object(self)

	val _health		= health
	val _hygiene	= hygiene
	val _energy		= energy
	val _happyness	= happyness

	val _action 	= 0


	method health = _health
	method hygiene = _hygiene
	method energy = _energy
	method happyness = _happyness
	method action = _action

	method warning 	 = (_health <= 20 || _hygiene <= 20 || _energy <= 20 || _happyness <= 20)
	method game_over = (_health = 0 || _hygiene = 0 || _energy = 0 || _happyness = 0)

	method get_old	= {< _health = self#range_meters (_health - 1);
					_hygiene = _hygiene;
					_energy = _energy;
					_happyness = _happyness; 
					_action = _action >}

	method clone	= {< _health = _health;
					_hygiene = _hygiene;
					_energy = _energy;
					_happyness = _happyness; 
					_action = _action >}

	method eat 		= {< _health = self#range_meters (_health + 25);
					_hygiene = self#range_meters (_hygiene - 20);
					_energy = self#range_meters (_energy - 10);
					_happyness = self#range_meters (_happyness + 5);
					_action = 1 >}

	method thunder 	= {< _health = self#range_meters (_health - 20);
					_hygiene = _hygiene;
					_energy = self#range_meters (_energy + 25);
					_happyness = self#range_meters (_happyness - 20);
					_action = 2 >}

	method bath 	= {< _health = self#range_meters (_health - 20);
					_hygiene = self#range_meters (_hygiene + 25);
					_energy = self#range_meters (_energy - 10);
					_happyness = self#range_meters (_happyness + 5);
					_action = 3 >}

	method kill 	= {< _health = self#range_meters (_health - 20);
					_hygiene = _hygiene;
					_energy = self#range_meters (_energy - 10);
					_happyness = self#range_meters (_happyness + 20);
					_action = 4 >}

	method sleep 	= {< _health = self#range_meters (_health + 20);
					_hygiene = self#range_meters (_hygiene - 5);
					_energy = self#range_meters (_energy + 50);
					_happyness = self#range_meters (_happyness + 10);
					_action = 5 >}

	method dance 	= {< _health = self#range_meters (_health + 10);
					_hygiene = self#range_meters (_hygiene - 10);
					_energy = self#range_meters (_energy - 10);
					_happyness = self#range_meters (_happyness + 20);
					_action = 6 >}

	method tickle 	= {< _health = _health;
					_hygiene = _hygiene;
					_energy = _energy;
					_happyness = self#range_meters (_happyness + 100);
					_action = 7 >}

	method private range_meters m:int = if (m >= 100) then 100 else if (m <= 0) then 0 else m;



end

