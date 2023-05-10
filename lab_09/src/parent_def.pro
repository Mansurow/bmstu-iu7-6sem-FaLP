DOMAINS
	name = string.
	gender = string.
	n = integer

PREDICATES
	isGender(name, gender).
	% 5 видов родственников
	% родители
	getParent(name, name).
	getMother(name, name).
	getFather(name, name).
	% бабушки и дедушки
	getGrandParent(name, name, gender).
	getGrandParentByMother(name, name, gender).
	getGrandParentByFather(name, name, gender).
	getGrandMother(name, name).
	getGrandFather(name, name).
	% тети и дяди
	getUA(name, name, gender).
	% сестры и братья
	getSibling(name, name, gender).
	% кузены
	getCousin(name, name, gender).
	% женаты
	isMarried(name, name). % последовательность - муж и жена
	% шурин или своячина
	getSiblingWife(name, name, gender).
	% золовка или деверь
	getSiblingHusband(name, name, gender).
	
	% бабушка n-го поколения
	getParentN(name, name, n, gender).
	getGrandParentN(name, name, n, gender).
CLAUSES
	isGender("CA", "male").
	isGender("MA", "female").
	isGender("FA", "male").
	isGender("MMA", "female").
	isGender("FMA", "male").
	isGender("MFA", "female").
	isGender("FFA", "male").
	isGender("MMMA", "female").
	isGender("FMMA", "male").
	isGender("MFMA", "female").
	isGender("FFMA", "male").
	isGender("MMFA", "female").
	isGender("FMFA", "male").
	isGender("MFFA", "female").
	isGender("FFFA", "male").

	isGender("Brother of Mother 1", "male").
	isGender("Brother of Mother 2", "male").
	isGender("Sister of Mother 1", "female").
	
	isGender("Sister1", "female").
	isGender("Brother1", "male").
	isGender("Brother2", "male").
	
	isGender("Cousin1", "male").
	isGender("Cousin2", "female").
	isGender("Cousin3", "female").
	
	isGender("CB", "male").
	isGender("MB", "female").
	isGender("FB", "male").
	% предки
	getParent("CA", "MA").
	getParent("CA", "FA").
	% предки родителей
	getParent("FA", "MFA").
	getParent("FA", "FFA").
	getParent("MA", "MMA").
	getParent("MA", "FMA").
	getParent("MMA", "MMMA").
	getParent("MMA", "FMMA").
	getParent("FMA", "MFMA").
	getParent("FMA", "FFMA").
	getParent("MFA", "MMFA").
	getParent("MFA", "FMFA").
	getParent("FFA", "FFFA").
	getParent("FFA", "MFFA").
	% сестра и братья родителей
	getParent("Brother of Mother 1", "MMA").
	getParent("Brother of Mother 1", "FMA").
	getParent("Sister of Mother 1", "MMA").
	getParent("Sister of Mother 1", "FMA").
	getParent("Sister of Father 1", "MFA").
	getParent("Sister of Father 1", "FFA").
	% сестры и братья
	getParent("Sister1", "MA").
	getParent("Brother1", "FA").
	getParent("Brother1", "MA").
	getParent("Brother2", "MA").
	getParent("Brother2", "FA").
	getParent("Sister2", "FA").
	% кузены
	getParent("Cousin1", "Brother of Mother 1").
	getParent("Cousin2", "Sister of Mother 1").
	getParent("Cousin3", "Sister of Father 1").
	
	getParent("CB", "MB").
	getParent("CB", "FB").
	
	isMarried("FA", "MA").
	isMarried("FB", "MB").
	isMarried("FMA", "MMA").
	isMarried("FFA", "MFA").	
		
	getMother(Child, Mother) :-
		getParent(Child, Mother),
		isGender(Mother, "female").
		
	getFather(Child, Father) :- 
		getParent(Child, Father),
		isGender(Father, "male").	
	
	getGrandMother(Child, GrandMother) :-
		getParent(Child, Parent),
		getParent(Parent, GrandMother),
		isGender(GrandMother, "female").
	
	getGrandFather(Child, GrandFather) :-
		getParent(Child, Parent),
		getParent(Parent, GrandFather),
		isGender(GrandFather, "male").
	
	getGrandParent(Child, GrandParent, GrandParentGender) :- 
		getParent(Child, Parent),
		getParent(Parent, GrandParent),
		isGender(GrandParent, GrandParentGender).
		
	getGrandParentByMother(Child, GrandParent, GrandParentGender) :- 
		getMother(Child, Parent),
		isGender(Parent, "female"),
		getParent(Parent, GrandParent),
		isGender(GrandParent, GrandParentGender).
	
	getGrandParentByFather(Child, GrandParent, GrandParentGender) :- 
		getParent(Child, Parent),
		isGender(Parent, "male"),
		getParent(Parent, GrandParent),
		isGender(GrandParent, GrandParentGender).	
	
	getSibling(Child, Sibling, Gender) :- 
		getParent(Child, Parent),
		getParent(Sibling, Parent),
		isGender(Sibling, Gender),
		not(Child = Sibling).
		
	getUA(Child, UA, Gender) :-
		getParent(Child, Parent),
		getSibling(Parent, UA, Gender).
		
	getCousin(Child, Cousin, Gender) :- 
		getUA(Child, UA, _),
		getParent(Cousin, UA),
		isGender(Cousin, Gender).
	
	getSiblingWife(Husband, Sibling, Gender) :-
		isMarried(Husband, Wife),
		getSibling(Wife, Sibling, Gender).
	
	getSiblingHusband(Wife, Sibling, Gender) :-
		isMarried(Husband, Wife),
		getSibling(Husband, Sibling, Gender).
		
	getParentN(Child, Parent, 1, Gender) :-
		getParent(Child, Parent),
		isGender(Parent, Gender).
			
	getParentN(Child, ParentN, N, Gender) :-
		New = N - 1,
		getParent(Child, Parent),
		getParentN(Parent, ParentN, New, Gender).
		
	getGrandParentN(Child, GrandParentN, N, Gender) :-
		getParent(Child, Parent),
		getParentN(Parent, GrandParentN, N, Gender).										 	
GOAL
	% Вывести имени родителей
	getParent("CA", Parent).
	% Вывести имя матери
	getMother("CA", Mother).
	% Вывести имя отца
	getFather("CA", Father).
	
	% Вывести имена бабушек 
	getGrandMother("CA", GrandMother).
	
	% Вывести имена дедушек
	getGrandFather("CA", GrandFather).
	
	% Вывести имена прородителей 
	getGrandParent("CA", GrandParent, Gender).
	getGrandParent("CA", GrandMother, "female").
	getGrandParent("CA", GrandFather, "male").
	
	% Вывести имена родителей по материнской линии
	getGrandParentByMother("CA", GrandParent, Gender).
	getGrandParentByMother("CA", GrandMother, "female").
	getGrandParentByMother("CA", GrandFather, "male").
	
	% Вывести имена родителей по отцовской линии
	getGrandParentByFather("CA", GrandParent, Gender).
	getGrandParentByFather("CA", GrandMother, "female").
	getGrandParentByFather("CA", GrandFather, "male").
	
	% найти всех сетер и братьев CA
	getSibling("CA", Sibling, _).	
	% Найти кузенов CA	
	getCousin("CA", Cousin, Gender).
	
	isMarried(Husband, Wife).
	% Сестры и братья по жене - FA
	getSiblingWife("FA", Sibling, _).
	% Сестры и братья по мужу - MA
	getSiblingHusband("MA", Sibling, _).

	% вывести всех предков n-го порядка
	getGrandParentN("CA", GrandParent, 2, Gender).	