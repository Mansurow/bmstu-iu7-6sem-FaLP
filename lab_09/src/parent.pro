DOMAINS
	name = string.
	gender = string.

PREDICATES
	parent(name, name, gender).
	% предки 2 поколения
	grandparent(name, name,  gender, gender).

CLAUSES
	parent("CA", "MA", "female").
	parent("CA", "FA", "male").
	parent("MA", "MMA", "female").
	parent("MA", "FMA", "male").
	parent("FA", "MFA", "female").
	parent("FA", "FFA", "male").
	
	grandparent(Child, GrandParent, ParentGender, GrandParentGender):- 
		parent(Parent, GrandParent, GrandParentGender),
		parent(Child, Parent, ParentGender).

GOAL
	% Вывести имена родиетелей по имени ребенка
	% parent("CA", Parent, Gender).
	% Вывести имена всех бабушек по имени ребенка
	% grandparent("CA", GrandParent, _, "female").
	% Вывести имена всех дедушек по имени ребенка
	% grandparent("CA", GrandParent, _, "male").
	% Вывести имена дедушки и бабушки по имени ребенка.
	% grandparent("CA", GrandParent, _, _).
	% Вывести имя бабушки по материнской линии 
	grandparent("CA", GrandParent, "female", "female").