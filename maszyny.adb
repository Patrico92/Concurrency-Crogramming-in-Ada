package body maszyny is

	protected body maszynaDodajaca is
    
    	entry zajmijMnie(sukces:out Boolean) when (not zajeta) is
    		begin
    		
    			zajeta := true;
    			sukces := true;
    			
    		end zajmijMnie; 
    	
    	procedure dodaj(X : in out Zadanie; success: out Boolean) is 
    		begin 
    		
    			delay Duration(czas_dodawania);
    			
    			if Random(G) <= prawd_zespucia_dod then
    				success := false;
    			else     			
		 			X.Wynik := X.Arg1 + X.Arg2;
		 			zajeta := false;
		 			success := true;
		 		end if;
    		
    		end dodaj;	
    		
    	entry naprawa when zajeta = true is	
    		begin
    				delay Duration(3.0); --czas trwania naprawy
    				zajeta := false;
		 	end naprawa;	
    	
	end maszynaDodajaca;
	
	protected body maszynaMnozaca is
	
		entry zajmijMnie(sukces : out Boolean) when iloscPracownikow = 0 and not zepsuta is
			begin
			
				iloscPracownikow := 1;
				sukces := true;
				
			end zajmijMnie;
		
		entry dolacz when iloscPracownikow = 1 and pomocnik = false is
			begin
				pomocnik := true;
				iloscPracownikow := 2;
			
			end dolacz;
			
		entry pomnoz(X : in out Zadanie; sukces: out Boolean) when iloscPracownikow = 2 is
			begin
			
				delay Duration(czas_mnozenia);
		 			
		 			if Random(G) <= prawd_zepsucia_mn then
		 				zepsuta := true;
		 				udanaOperacja := false;
			 			sukces := false;
		 				iloscPracownikow := 1; 
		 			else    			
			 			X.Wynik := X.Arg1 * X.Arg2;
			 			udanaOperacja := true;
			 			sukces := true;
			 			iloscPracownikow := 1;
			 		end if;    						
			
			end pomnoz;
		
		entry zwolnijMnie(sukces : out Boolean) when iloscPracownikow = 1 is
			begin
				
				sukces := udanaOperacja;
				iloscPracownikow := 0;
				pomocnik := false;
			
			end zwolnijMnie;
			
		entry naprawa when iloscPracownikow = 0 is
		begin
		
			delay Duration(3.0);
			zepsuta := false;
			
		end naprawa;
	end maszynaMnozaca;

end maszyny;







