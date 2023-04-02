domains
	name, surname=string.
	phone_list=string*.
	city, street=string.
	house, flat=integer.
	address=address_data(city, street, house, flat).
	price, car_number=integer.
	colour, brand=string.

predicates
	phone_record(surname, name, address, phone_list).
	car(surname, name, brand, colour, price, car_number).

clauses
	car("Иванов", "Александр", "Mercedes", "black", 200000, 2313).
	car("Иванов", "Виктор", "Mercedes", "yellow", 200000, 4312).
	car("Сидоров", "Александр", "Lada", "red", 50000, 5555).
	car("Жаров", "Андрей", "Toyta", "green", 100000, 9999).
	car("Петров", "Евгений", "Audi", "gray", 600000, 8888).
	car("Петров", "Евгений", "Mercedes", "black", 500000, 7788).
	 

	phone_record("Иванов", "Александр", address_data("Москва", "Абельмановская улица", 10, 20), ["+89280202902", "+891904802", "+891904805"]).
	phone_record("Иванов", "Виктор", address_data("Новосибирск", "Авиаматорная улица", 1, 10), ["8456372"]).
	phone_record("Сидоров", "Александр", address_data("Москва", "Анненский проезд", 3, 15), ["8994527"]).
	phone_record("Жаров", "Андрей", address_data("Москва", "улица Носова", 1, 16), ["8994558"]).
	phone_record("Петров", "Евгений", address_data("Москва", "улица Петра", 5, 10), ["6994566"]).

goal
	% Какую фамилию, номер, город, имеет владелец автомобиля конкретной марки и цвета автомобиля?
	phone_record(Surname, _, address_data(City, _, _, _), Phone),
	car(Surname, _,"Mercedes", "black", _, _).

	% Какую фамилию, марку, номер машины имеют жители москвы?
	phone_record(Surname, _, address_data("Москва", _, _, _), _),
	car(Surname, _, Mark, _, _, CarNumber).
	
	% Если автомобиль у  Жарова Андрей маркой Toyta?
	car("Жаров", _, "Toyta", _, _, _).
	  
	% Какая фамилия и имя владельца у марки автобиля "Mercedes"?
	car(Surname, Name,  "Mercedes", _, _, _).  