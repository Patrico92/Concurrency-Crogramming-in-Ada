with pakietstalych; use pakietstalych;
with Ada.Text_IO; use Ada.Text_IO;

package listaImagazyn is


	protected type listaZadan is 

			entry dodajZadanie(X: Zadanie);
			entry pobierzZadanie(X : out Zadanie);

		private
	
			S : Zadania (1 .. max_ilosc_zadan);
			wierzcholek: Integer := 0;
			potrzebnaPomoc : Boolean := false;
			pomocniczeZadanie : Zadanie;

	end listaZadan;
	
	protected type magazyn is
		
			entry dodajProdukt(X : Zadanie);
			function ileWMagazynie return Integer;
			procedure pobierzZawartosc(produkty : out Zadania);
			
		private
		
			S : Zadania ( 1 .. wielkosc_magazynu);
			wierzcholek : Integer := 1;
			
	end magazyn;

end listaimagazyn;
