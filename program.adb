with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with pakietstalych; use pakietstalych;
with Ada.Text_IO; use Ada.Text_IO;
with Random_Seeds; use Random_Seeds; -- bilbioteki prowadzącego
with maszyny; use maszyny;
with listaimagazyn; use listaimagazyn;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Integer_Text_IO; use Ada;


procedure program is

	v : Boolean := false;
	N : Integer := pakietStalych.ilosc_klientow + 1;
	Seeds : Seed_Array_Type(1 .. N) := Make_Seeds(N); -- generujemy zalążki do generatorów
	
	type wsk_maszyna_dod is access all maszynaDodajaca;
	type wsk_maszyna_mn is access all maszynaMnozaca;
	type Tablica_mnozacych is array (Integer range<>) of wsk_maszyna_mn;
	type Tablica_dodajacych is array (Integer range<>) of wsk_maszyna_dod;
	type Tablica_Maszyn_Dodajacych is array ( Integer range <> ) of aliased maszynaDodajaca;		
	type Tablica_Maszyn_Mnozacych is array ( Integer range <> ) of aliased maszynaMnozaca;
	type Zadania_Wsk is access all Zadania;
	
	
---------------------------------------------------------------------------------------------------Sklep	


	protected type Sklep (ID: Integer) is
	
		entry kupProdukt(op : Operacja; X : out Zadanie; sukces : out Boolean);
		entry ileBrakuje(dod,od,mn : out Integer);
		procedure ileTowaru;
		procedure Dostawa(X: Zadania);
		procedure koordynaty(x,y : out Integer);

	private
		
		Mnozenia : Zadania (1 .. ilosc_towarow_w_sklepie);
		pointerMnozenia : Integer := 0;
		Dodawania: Zadania (1 .. ilosc_towarow_w_sklepie);
		pointerDodawania : Integer := 0;
		Odejmowania: Zadania (1 .. ilosc_towarow_w_sklepie);
		pointerOdejmowania: Integer := 0;
		pomocniczeZadanie : Zadanie;
		dostawaWToku : boolean := false;
		G: Generator;
		wspX : Integer := -1;
		wspY : Integer := -1;
		mySeed : Seed_Array_Type(1 .. 1) := Make_Seeds(1);
		
		
		
	end Sklep;
	
	protected body Sklep is
	
	procedure ileTowaru is
	begin
		Put_Line("Sklep" & Integer'Image(ID) & ": mamy na stanie" & Integer'Image(pointerDodawania) & Integer'Image(pointerOdejmowania) & Integer'Image(pointerMnozenia));
	end;
	
		entry kupProdukt(op : Operacja; X : out Zadanie; sukces : out Boolean) when true is
			begin
				case op is
					when dodaj =>
						if pointerDodawania > 0 then
						
							X := Dodawania(pointerDodawania);
							sukces := true;
							pointerDodawania := pointerDodawania - 1;
							
						else
						
							X := pomocniczeZadanie;
							sukces := false;
							
						end if;
					when odejmij => 
						if pointerOdejmowania > 0 then
							X := Odejmowania(pointerOdejmowania);
							sukces := true;
							pointerOdejmowania := pointerOdejmowania - 1;
						
						else
							X := pomocniczeZadanie;
							sukces := false;
						end if;
						
					when pomnoz =>
						if pointerMnozenia > 0 then
							X := Mnozenia(pointerMnozenia);
							sukces := true;
							pointerMnozenia := pointerMnozenia - 1;

						else
							X := pomocniczeZadanie;
							sukces := false;
						end if;
					when pomoc => null;	
					end case;
			
			end kupProdukt;
			
		entry ileBrakuje(dod,od,mn : out Integer)
			when dostawaWToku = false is
		begin
		
			dod := ilosc_towarow_w_sklepie - pointerDodawania;
			od := ilosc_towarow_w_sklepie - pointerOdejmowania;
			mn := ilosc_towarow_w_sklepie - pointerMnozenia;	
			
			if dod+od+mn > 0 then dostawaWToku := true; end if;
		
		end ileBrakuje;
			
		procedure Dostawa(X: Zadania) is
		begin
			
			for i in 1 .. X'Length loop
				case X(i).op is
					when dodaj =>
						pointerDodawania := pointerDodawania + 1;
						Dodawania(pointerDodawania) := X(i);
					when odejmij =>
						pointerOdejmowania := pointerOdejmowania + 1;
						Odejmowania(pointerOdejmowania) := X(i);
					when pomnoz =>
						pointerMnozenia := pointerMnozenia + 1;
						Mnozenia(pointerMnozenia) := X(i);
					when others => null;
				end case;	
			
			end loop;
			
			dostawaWToku := false;	
			if v then Put_Line("Sklep" & Integer'Image(ID) & ": dostawa przyjeta!"); end if;	
		
		end Dostawa;
		
		procedure koordynaty(x,y : out Integer) is
		begin
		
			if wspX = -1 then
				Reset(G,mySeed(1));
				wspX := Integer(Random(G)*10.0);
				wspY := Integer(Random(G)*10.0);
			end if;
			
			x := wspX;
			y := wspY;
		
		end koordynaty;

	end Sklep;
	
	type tablica_Sklepow is array ( Positive range <>) of access Sklep;
	type tablica_Sklepow_wsk is access tablica_Sklepow;
	
	function stworz_Sklepy return tablica_Sklepow_wsk is
  		wsk: tablica_Sklepow_wsk ;
			begin
				wsk := new tablica_Sklepow( 1 .. ilosc_sklepow);
	 				for I in wsk.all'Range loop
	 				wsk.all(I) := new Sklep(I);
			end loop;
	   return wsk;
	end;
	
	tablicaSklepow : tablica_sklepow := stworz_Sklepy.all;	
	
---------------------------------------------------------------------------------------------------infoFabryka

	protected type infoFabryka(ID: Integer) is
	
		entry zamow(dod,od,mn : Integer; sukces : out Boolean);
		entry pobierzZamowienie(dod,od,mn : out Integer);
		entry przechowalnia(X : Zadania_Wsk);
		entry wydajProdukty( X : out Zadania_Wsk);
		procedure koordynaty(x,y : out Integer);
		procedure jakieZamowienie;
		
		private
		
			zamowienieWToku : boolean := false;
			dodawania, odejmowania, mnozenia : Integer;
			produkty: Zadania_Wsk;
			gotowe : boolean := false;
			G: Generator;
			wspX : Integer := -1;
			wspY : Integer := -1;
			mySeed : Seed_Array_Type(1 .. 1) := Make_Seeds(1);
		
	end infoFabryka;	
	
		protected body infoFabryka is
		
		procedure jakieZamowienie is
		begin
			if zamowienieWToku = true then
				Put_Line("Fabryka" & Integer'Image(ID) & ": mamy zamowienie na" & Integer'Image(dodawania) & Integer'Image(odejmowania) & Integer'Image(mnozenia));
			else
				Put_Line("Fabryka" & Integer'Image(ID) & ": nie mamy żadnego zamowienia :(");			
			end if;
		end;
	
		entry zamow(dod,od,mn : Integer; sukces: out Boolean) 
			when zamowienieWToku = false is
		begin
		
			zamowienieWToku := true;
			
			dodawania := dod;
			odejmowania := od;
			mnozenia := mn;
			sukces := true;
		
		end zamow;
		
		entry pobierzZamowienie(dod,od,mn : out Integer) 
			when zamowienieWToku = true is
		begin
		
			dod := dodawania;
			od := odejmowania;
			mn := mnozenia;
			
		end pobierzZamowienie;
		
		entry przechowalnia(X : Zadania_Wsk) 
			when zamowienieWToku = true is
		begin
			produkty := X;
			gotowe := true;
			
		end przechowalnia;
		
		entry wydajProdukty( X : out Zadania_Wsk) when gotowe = true is
		begin
			zamowienieWToku := false;
			X := produkty;
			gotowe := false;
		end wydajProdukty;
		
		procedure koordynaty(x,y : out Integer) is
		begin
		
			if wspX = -1 then
				Reset(G,mySeed(1));
				wspX := Integer(Random(G)*10.0);
				wspY := Integer(Random(G)*10.0);
			end if;
			
			x := wspX;
			y := wspY;
		
		end koordynaty;
		
	
	end infoFabryka;
	
	tablicaInfoFabryk : array (1 .. ilosc_fabryk) of access infoFabryka;
	
-------------------------------------------------------------------------------------------------------------------Fabryka

	task type Fabryka(ID: Integer);
	

	task body Fabryka is
	
		mojeProdukty : Zadania_Wsk;
	
		N : Integer := pakietStalych.ilosc_pracownikow;
		Seeds : Seed_Array_Type(1..N) := Make_Seeds(N); -- generujemy zalążki do generatorów
		X: Wsk_Zadanie;
		GR: Generator;
		produkcjaWToku : Boolean := false;
			
		Maszyny_Dodajace :  Tablica_Maszyn_Dodajacych (1..pakietStalych.ilosc_maszyn_dodajacych);
		Maszyny_Mnozace :  Tablica_Maszyn_Mnozacych (1..pakietStalych.ilosc_maszyn_mnozacych);
		
		MojaListaZadan : listaZadan;
		MojMagazyn : magazyn;
		
		protected CentralaAwarii is
	
			procedure dodajPowiadomienieDodajaca(n: in Integer; Maszyna: in wsk_maszyna_dod);
			procedure dodajPowiadomienieMnozaca(n: in Integer; Maszyna: in wsk_maszyna_mn);
			procedure coDoNaprawyDodajace(Maszyna: out wsk_maszyna_dod; nr: out Integer);
			procedure coDoNaprawyMnozace(Maszyna: out wsk_maszyna_mn; nr: out Integer);
		
		private 
		
			TM : Tablica_mnozacych (1 .. pakietStalych.ilosc_maszyn_mnozacych);
			TD : Tablica_dodajacych (1 .. pakietStalych.ilosc_maszyn_dodajacych);
		
		end CentralaAwarii;
	
		protected body CentralaAwarii is
		
		procedure dodajPowiadomienieDodajaca(n: in Integer; Maszyna: in wsk_maszyna_dod) is
			begin
				TD(n) := Maszyna;
			end dodajPowiadomienieDodajaca;
			
		procedure dodajPowiadomienieMnozaca(n: in Integer; Maszyna: in wsk_maszyna_mn) is
			begin
				TM(n) := Maszyna;
			end dodajPowiadomienieMnozaca;
			
		procedure coDoNaprawyDodajace(Maszyna: out wsk_maszyna_dod; nr: out Integer) is 
			begin
				for I in 1..pakietStalych.ilosc_maszyn_dodajacych loop
					if TD(I) /= null then Maszyna := TD(I);
						TD(I) := null;
						nr := I;
						return;
					end if;					
				end loop;
				
				nr := 0;
			end coDoNaprawyDodajace;
			
		procedure coDoNaprawyMnozace(Maszyna: out wsk_maszyna_mn; nr: out Integer) is
			begin
				for I in 1..pakietStalych.ilosc_maszyn_mnozacych loop
					if TM(I) /= null then Maszyna := TM(I);
						TM(I) := null;
						nr := I;
						return;
					end if;					
				end loop;
				
				nr := 0;
			end coDoNaprawyMnozace;
	
	end CentralaAwarii;
	
	task Serwisant;
		
	task body Serwisant is 
	
		Maszyna : wsk_maszyna_dod;
		Maszyna2 : wsk_maszyna_mn;
		nr : Integer;
		
	
		begin
			loop
			
				delay Duration(1.0);
				
				CentralaAwarii.coDoNaprawyDodajace(Maszyna, nr);
				if nr/=0 then 
					Maszyna.Naprawa;
					
				end if;
				
				CentralaAwarii.coDoNaprawyMnozace(Maszyna2, nr);
				if nr/=0 then 
					Maszyna2.Naprawa;
				end if;			
						
			end loop;
	
	end Serwisant;
	
	task type Pracownik(ID : Integer);

task body Pracownik is

	X: Zadanie;
	G: Generator;
	sukces: Boolean;
	MojaMaszynaMn : wsk_maszyna_mn;
	MojaMaszynaDodajaca : wsk_maszyna_dod;
	I: Integer;			

begin  

	Reset(G, Seeds(ID));
	
		loop
			delay 0.1 + Duration(Random(G)*pakietStalych.stala_pracownikow);
				
				MojaListaZadan.pobierzZadanie(X);
				
				
				case X.Op is
					when pomoc => 
					
					<<pomagam_dalej>>
					sukces := false;
					
					
					I := 0;
					
						while not sukces loop							
						
							I := I+1;
							
							if I > pakietStalych.ilosc_maszyn_mnozacych then I := 1; end if;
						
							select Maszyny_Mnozace(I).dolacz;
									MojaMaszynaMn := Maszyny_Mnozace(I)'Unrestricted_Access;
									sukces := true;
							else null;
							end select;
							
						end loop;
						
						MojaMaszynaMn.zwolnijMnie(sukces);
						
						if not sukces then
							goto pomagam_dalej;
						end if;
						
			
					when dodaj => 
					
						<<popsuta_maszyna>> -- tutaj trafiamy gdyby w trakcie wykonywania pracy maszyna się popsuła
						sukces := false;
						
						I := 0;
						
						while not sukces loop
						
							I := I+1;
							if I > pakietStalych.ilosc_maszyn_dodajacych then	I := 1; end if;
						
							select Maszyny_Dodajace(I).zajmijMnie(sukces);
								MojaMaszynaDodajaca := Maszyny_Dodajace(I)'Unrestricted_Access;
							else null;
							end select;
							
						end loop;
						
						
						MojaMaszynaDodajaca.dodaj(X,sukces);
						
						if not sukces then
							CentralaAwarii.dodajPowiadomienieDodajaca(I, MojaMaszynaDodajaca);
							goto popsuta_maszyna;
						end if;
												
						
					when pomnoz => 
					
						<<popsuta_maszyna_mn>>
						sukces := false;
						
						I := 0;
						
							while not sukces loop							
						
								I := I+1;
								if I > pakietStalych.ilosc_maszyn_mnozacych then I := 1; end if;
								
								select Maszyny_Mnozace(I).zajmijMnie(sukces);
									MojaMaszynaMn := Maszyny_Mnozace(I)'Unrestricted_Access;
								else null;
								end select;
							
							end loop;
						
	
							MojaMaszynaMn.pomnoz(X, sukces);
						
							if not sukces then
								CentralaAwarii.dodajPowiadomienieMnozaca(I, MojaMaszynaMn);
								goto popsuta_maszyna_mn;
							end if;
																		
					when odejmij =>
						X.Wynik := X.Arg1 - X.Arg2;		
							
					when others => null;	
								
				end case;

					if X.Op /= pomoc then
				
					MojMagazyn.dodajProdukt(X);
					
					end if;

		end loop;
	end Pracownik;
	
	type tablica_Pracownikow is array ( Positive range <>) of access Pracownik;
	type tablica_Pracownikow_wsk is access tablica_Pracownikow;


	function stworz_Pracownikow return tablica_Pracownikow_wsk is
   wsk: tablica_Pracownikow_wsk ;
			begin
				wsk := new tablica_Pracownikow( 1 .. pakietStalych.ilosc_pracownikow);
	 				for I in wsk.all'Range loop
	 				wsk.all(I) := new Pracownik(I);
			end loop;
	   return wsk;
	end;
	

	TablicaPracownikow : tablica_Pracownikow := stworz_Pracownikow.all;
	
	dod,od,mn, wielkoscZamowienia : Integer;
	
	
	begin
		if v then Put_Line("Fabryka" & Integer'Image(ID) & " gotowa!"); end if;
		loop
				
				
				tablicaInfoFabryk(ID).pobierzZamowienie(dod,od,mn);
				
				wielkoscZamowienia := dod+od+mn;				
					
				if v then Put_Line("Fabryka" & Integer'Image(ID) & " przyjmuje zamówienie..."); end if;
				
						for i in 1 .. dod loop
					
							X := new Zadanie;
							X.Op := dodaj;
							X.Arg1 := Random(GR);
							X.Arg2 := Random(GR);
						
							MojaListaZadan.dodajZadanie(X.all);					
						
						end loop;
						
						if v then Put_Line("Fabryka" & Integer'Image(ID) & " przyjmuje zamówienie dodawania..."); end if;
					
						for i in 1 .. od loop
					
							X := new Zadanie;
							X.Op := odejmij;
							X.Arg1 := Random(GR);
							X.Arg2 := Random(GR);
						
							MojaListaZadan.dodajZadanie(X.all);						
						
						end loop;
						
						if v then Put_Line("Fabryka" & Integer'Image(ID) & " przyjmuje zamówienie odejmowania..."); end if;
					
						for i in 1 .. mn loop
					
							X := new Zadanie;
							X.Op := pomnoz;
							X.Arg1 := Random(GR);
							X.Arg2 := Random(GR);
						
							MojaListaZadan.dodajZadanie(X.all);						
						
						end loop;
						
					if v then Put_Line("Fabryka" & Integer'Image(ID) & " przyjelismy zamowienie!"); end if;
					
					while MojMagazyn.ileWMagazynie < wielkoscZamowienia loop
						delay 1.0;
						if v then 
						Put_Line("Fabryka" & Integer'Image(ID) & ": skompletowalismy juz" & Integer'Image(MojMagazyn.ileWMagazynie) & " na" & Integer'Image(wielkoscZamowienia)); 
						end if;
					end loop;
					
					mojeProdukty := new Zadania(1 .. wielkoscZamowienia);
					
					
					MojMagazyn.pobierzZawartosc(mojeProdukty.all);
					
					tablicaInfoFabryk(ID).przechowalnia(mojeProdukty);

						
		end loop;
	end Fabryka;
	

	tablicaFabryk: array(1 .. ilosc_fabryk) of access Fabryka;
			
------------------------------------------------------------------------------------------------------------firmaTransportowa	

	protected type firmaTransportowa(ID : Integer) is
	
		entry zamow(sklepID,dod,od,mn: Integer; sukces : out  boolean); 
		entry pobierzZamowienie(sklepID,dod,od,mn: out Integer);
		procedure zamowienieZrealizowane;
		procedure firmaInfo;
		
		private
		
			realizujeZamowienie : Boolean := false;
			idSklepu : Integer := 0;
			dodawania, odejmowania, mnozenia : Integer;
		
	end firmaTransportowa;
	
	protected body firmaTransportowa is
	
		procedure firmaInfo is
		begin
			if idSklepu /= 0 then
				Put_Line("Firma transportowa" & Integer'Image(ID) & ": obsluga zamowienia" & Integer'Image(dodawania) & Integer'Image(odejmowania) & integer'Image(mnozenia) & " dla sklepu" & Integer'Image(idSklepu));
			else
				Put_Line("Firma transportowa" & Integer'Image(ID) & ": nie ma zamowien!");
			end if;
			end firmaInfo;
		
		entry zamow(sklepID,dod,od,mn : Integer; sukces : out boolean)
		 when realizujeZamowienie = false is
		begin
			realizujeZamowienie := true;
			idSklepu := sklepID;
			sukces := true;
			dodawania := dod;
			odejmowania := od;
			mnozenia := mn;
			
			if v then Put_Line("Firma transportowa" & integer'Image(ID) & ": przyjeto zamowienie ze sklepu" & Integer'image(idSklepu) & " na"  & Integer'Image(dod) & Integer'Image(od) & integer'Image(mn)); end if;
			
		end zamow;
		
		entry pobierzZamowienie(sklepID,dod,od,mn: out Integer) when realizujeZamowienie = true is
		begin
			sklepID := idSklepu;
			dod := dodawania;
			od := odejmowania;
			mn := mnozenia;

		end pobierzZamowienie;
		
		procedure zamowienieZrealizowane is
		begin
			realizujeZamowienie := false;
			idSklepu := 0;
		end zamowienieZrealizowane;
	
	end firmaTransportowa;
	
	tablicaFirmTransportowych: array(1 .. ilosc_firm_transportowych) of access firmaTransportowa;
	
	
-----------------------------------------------------------------------------------------------------------------------Ciezarowka	
	
	task type Ciezarowka(ID: Integer);
	task body Ciezarowka is
		
		sklepID, dod, od, mn, i : Integer;
		sukces : boolean := false;
		X : Zadania_Wsk;
		x1,x2,y1,y2 : Integer;
		
		distance : Float;
		
	begin
		loop
		
			tablicaFirmTransportowych(ID).pobierzZamowienie(sklepID,dod,od,mn);
			
			i := 0;
			
			while sukces = false loop
			
				delay 1.0;
			
				i := i + 1;
				if i > ilosc_fabryk then i := 1; end if;
				
				select tablicaInfoFabryk(i).zamow(dod,od,mn,sukces);
				else null;
				end select;
			
			end loop;
			
			sukces := false;
			
			if v then Put_Line("Zamowienie sklepu" & Integer'Image(sklepID) & " złożone do fabryki" & Integer'Image(i)); end if;
			
			tablicaInfoFabryk(i).wydajProdukty(X);
			
			tablicaInfoFabryk(i).koordynaty(x1,y1);
			tablicaSklepow(sklepID).koordynaty(x2,y2);
			
			distance := Float(x1+x2+y1+y2);
			
			delay Duration(distance/5.0);
			
			
			if v then Put_Line("Zamowienie sklepu" & Integer'Image(sklepID) & " w drodze! Odległość:" & Float'Image(distance/5.0)); end if;
			
			tablicaSklepow(sklepID).dostawa(X.all);	
			
			
			tablicaFirmTransportowych(ID).zamowienieZrealizowane;
		
		
		end loop;
	end Ciezarowka;
	
	tablicaCiezarowek: array(1 .. ilosc_firm_transportowych) of access Ciezarowka;
	
	-----------------------------------------------------------------------------------------------------------Sklep
	
	

------------------------------------------------------------------------------------------------Klient

	task type Klient (ID: Integer);
	task body Klient is

	G: Generator;
	sklepID : Integer;
	oczekiwanaOperacja : Operacja;
	X : Zadanie;
	sukces: Boolean := false;
	
	begin
	
		Reset(G, Seeds(ID));
		
		
		loop
		
			delay 0.1 + Duration(stala_klientow*Random(G));
			
			--odbywa się losowanie, do którego sklepu pójdzie klient i wynik jakiej operacji go interesuje
			
			sklepID := (Integer(Random(G)*100.0) mod ilosc_sklepow) + 1;
			oczekiwanaOperacja :=  Operacja'Val( Integer(Random(G)*100.0) mod 3);
						
			tablicaSklepow(sklepID).kupProdukt(oczekiwanaOperacja, X, sukces);
			
			if sukces = false then
			
				if v then Put_Line("Klient" & Integer'Image(ID) & ": W sklepie" & Integer'Image(sklepID) & " nie mają produktu " & Operacja'Image(oczekiwanaOperacja) & "! Wychodzę!"); end if;
			 else 
			  	if v then Put_Line("Klient" & Integer'Image(ID) & ": W sklepie" & Integer'Image(sklepID) & " kupiłem produkt " & Operacja'Image(X.op) & Float'Image(X.Arg1) & Float'Image(X.Arg2) & Float'Image(X.Wynik)); end if;
			 end if;
					
		
		end loop;
	

	end Klient;
	
	type tablica_Klientow is array ( Positive range <>) of access Klient;
	type tablica_Klientow_wsk is access tablica_Klientow;


	function stworz_Klientow return tablica_Klientow_wsk is
   wsk: tablica_Klientow_wsk ;
			begin
				wsk := new tablica_Klientow( 1 .. ilosc_klientow);
	 				for I in wsk.all'Range loop
	 				wsk.all(I) := new Klient(I);
			end loop;
	   return wsk;
	end;
	
	
	TablicaKlientow : tablica_klientow := stworz_Klientow.all ;
	
	------------------------------------------------------------------------------------------------Kierownik Sklepu

	task type Kierownik (ID: Integer);
		
	task body Kierownik is
	
		Seed : Seed_Array_Type(1 .. 1) := Make_Seeds(1); -- generujemy zalążki do generatorów
		G: Generator;
		dod,od,mn,i : Integer;
		sukces : Boolean := false;
		
			begin
			Reset(G,Seed(1));
				loop 
					delay Duration(Random(G)*stala_kierownika);
					
						tablicaSklepow(ID).ileBrakuje(dod,od,mn);
						
						if dod+od+mn > 0 then
												
							i := 0;
							while sukces = false loop
						
								i := i + 1;
							
								if i > ilosc_firm_transportowych 
									then i := 1; 
								end if;
							
								select tablicaFirmTransportowych(i).zamow(ID,dod,od,mn,sukces);
								else null;
								end select; 
								
								delay 1.0;
							
							end loop;
						
							sukces := false;
						
							if v then Put_Line("Kierownik sklepu" & Integer'Image(ID) & ": zlozylem zamowienie do firmy transportowej" & Integer'Image(i) & " na"  & Integer'Image(dod) & Integer'Image(od) & integer'Image(mn)); end if;
						
						end if;
						
						
											
					
				end loop;
	end Kierownik;
	
	tablicaKierownikow: array(1 .. ilosc_sklepow) of access Kierownik;
	
	task type Info;

	task body Info is
	
		opcja : Integer;
		x,y : Integer;
		
	begin
		loop		
		
		Put_Line("Dostępne opcje: ");
		Put_Line("1 - ilosc towarow w sklepach");
		Put_Line("2 - zamowienia do fabryk");
		Put_Line("3 - firmy transportowe - informacje");
		Put_Line("4 - współrzędne sklepów i fabryk");
		Integer_Text_IO.Get(opcja);
		Put_Line("");
	
		case opcja is
			when 1 => 
			
				for i in 1 .. ilosc_sklepow loop
					tablicaSklepow(i).ileTowaru;
				end loop;
				
			when 2 => 
			
				for i in 1 .. ilosc_fabryk loop
					tablicaInfoFabryk(i).jakieZamowienie;
				end loop;
				
			when 3 => 
			
				for i in 1 .. ilosc_firm_transportowych loop
					tablicaFirmTransportowych(i).firmaInfo;
				end loop;
				
			when 4 => 
			
				for i in 1 .. ilosc_sklepow loop
						tablicaSklepow(i).koordynaty(x,y);
						Put_Line("Sklep" & Integer'Image(i) & ": " & Integer'Image(x) & Integer'Image(y));	
					end loop;
					
				for i in 1 .. ilosc_fabryk loop
					tablicaInfoFabryk(i).koordynaty(x,y);
					Put_Line("Fabryka" & Integer'Image(i) & ": " & Integer'Image(x) & Integer'Image(y));	
				end loop;
			when others => 
				Put_Line("Opcja niedostępna, wybierz jedną z poniższych");
		end case;
		
	Put_Line("");
		
		
		
		end loop;
	end Info;
	
	type infoWsk is access Info;
	Info1: infoWsk;
	
	
	
begin

	if Argument_Count = 1 then
		if Argument(1) = "verbose" then 
			v := true;
		elsif Argument(1) = "quiet" then
			v := false;
			Info1 := new info;
		end if; 
	end if;

	
	--tworzenie fabryk
	for i in 1 .. ilosc_fabryk loop
		tablicaFabryk(i) := new Fabryka(i);
		tablicaInfoFabryk(i) := new infoFabryka(i); 
	end loop;
	
	for i in 1 .. ilosc_firm_transportowych loop
		tablicaFirmTransportowych(i) := new firmaTransportowa(i);
	end loop;	
	
	for i in 1 .. ilosc_firm_transportowych loop
		tablicaCiezarowek(i) := new Ciezarowka(i);
	end loop;	
	
	for i in 1 .. ilosc_sklepow loop
		tablicaKierownikow(i) := new Kierownik(i);
	end loop;	
	
	
end program;



