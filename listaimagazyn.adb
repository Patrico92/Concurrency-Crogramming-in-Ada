package body listaImagazyn is

	protected body listaZadan is

		entry dodajZadanie(X : Zadanie) when wierzcholek < max_ilosc_zadan is
			begin
		
				wierzcholek := wierzcholek + 1;
				S(wierzcholek) := X;
				
		
			end dodajZadanie;
		
		entry pobierzZadanie(X : out Zadanie) when wierzcholek > 0 or potrzebnaPomoc is
			begin
		
				if potrzebnaPomoc then
			
					pomocniczeZadanie.Op := pomoc;
					X := pomocniczeZadanie;
					potrzebnaPomoc := false;
			
				else
		
					
					X := S(wierzcholek);
			
					if X.Op = pomnoz then
						potrzebnaPomoc := true;
						pomocniczeZadanie := X;
					end if;
					
					wierzcholek := wierzcholek - 1;
			
				end if;
				
			end pobierzZadanie;
			
		

	end listaZadan;
	
	protected body magazyn is
	
		entry dodajProdukt(X: Zadanie) when wierzcholek <= wielkosc_magazynu is
			begin
			
				S(wierzcholek) := X;
				wierzcholek := wierzcholek + 1;
			
			end dodajProdukt;
			
		function ileWMagazynie return Integer is
			begin
			return wierzcholek - 1;
		end ileWMagazynie;
		
		procedure pobierzZawartosc(produkty : out Zadania) is
			begin
				for i in 1 .. (wierzcholek -1) loop
					produkty(i) := S(i);
				end loop;
				
				wierzcholek := 1;
			end pobierzZawartosc;
		
		
			
	end magazyn;
	
end listaimagazyn;







