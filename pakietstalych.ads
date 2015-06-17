package pakietstalych is

	max_ilosc_zadan : constant Integer := 5;
	wielkosc_magazynu : constant Integer := 15;
	stala_kierownika : constant Float := 3.0;
	stala_pracownikow : constant Float := 3.0;
	stala_klientow : constant Float := 6.0;
	ilosc_pracownikow : constant Integer := 3;
	ilosc_klientow : constant Integer := 2;
	ilosc_sklepow : constant Integer := 3;
	ilosc_fabryk : constant Integer := 2;
	ilosc_towarow_w_sklepie : constant Integer := 5;
	ilosc_firm_transportowych : constant Integer := 2;
	
	
	ilosc_maszyn_dodajacych : constant := 2;
	ilosc_maszyn_mnozacych : constant := 1;
	
	czas_dodawania : constant := 1.0;
	czas_mnozenia : constant := 1.2;
	
	prawd_zespucia_dod : constant := 0.05;
	prawd_zepsucia_mn : constant := 0.05;
	
	type Operacja is (dodaj,pomnoz,odejmij,pomoc);
	type Zadanie is
		record
			Arg1: Float;
			Arg2: Float;
			Op: Operacja;
			Wynik: Float;
		end record;
		
	type Wsk_Zadanie is access all Zadanie;
	type Zadania is array (Integer range <>) of aliased Zadanie;
	
end pakietstalych;
