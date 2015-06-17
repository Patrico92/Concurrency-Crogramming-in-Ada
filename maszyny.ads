with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with pakietstalych; use pakietstalych;

package maszyny is

	protected type maszynaDodajaca is
	
		entry zajmijMnie(sukces : out Boolean);
		procedure dodaj (X: in out Zadanie; success: out Boolean);
		entry naprawa;
    	
    	private
    	
    			zajeta : Boolean := false;  
    			G: Generator;
    			 
	end maszynaDodajaca;
	
	protected type maszynaMnozaca is
	
		entry zajmijMnie(sukces : out Boolean);
    	entry dolacz;
    	entry pomnoz(X : in out Zadanie; sukces: out Boolean);
    	entry zwolnijMnie(sukces : out Boolean);
    	entry naprawa;
    	
    	private
    	
    			zepsuta : Boolean := false;
    			pomocnik : Boolean := false;
    			udanaOperacja : Boolean := true;
    			iloscPracownikow : Integer := 0;
    			G: Generator;
    			 
	end maszynaMnozaca;


end maszyny;
