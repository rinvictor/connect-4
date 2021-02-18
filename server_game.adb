with Ada.Strings.Unbounded;

package body server_game is 

	use type ASU.Unbounded_String;
	use type LLU.End_Point_Type;

	procedure Set_Player_Info(C4_Game: in out C4_Game_Type; Nick: in ASU.Unbounded_String; 
								EP: in LLU.End_Point_Type) is
	begin 
		C4_Game.Current_Players := C4_Game.Current_Players + 1;
		C4_Game.Player_Info(C4_Game.Current_Players).Nick := Nick;
		C4_Game.Player_Info(C4_Game.Current_Players).EP := EP;
	end Set_Player_Info;

	function Get_Dashboard(C4_Game: in C4_Game_Type) return access VD.Board_Type is
	begin
		return C4_Game.Dashboard;
	end Get_Dashboard;

	function Get_Client_EP(C4_Game: in C4_Game_Type; Client: Integer) return LLU.End_Point_Type is
	begin
		return C4_Game.Player_Info(Client).EP;
	end;

	function Get_Client_Name(C4_Game: in C4_Game_Type; Client: Integer) return ASU.Unbounded_String is
	begin
		return C4_Game.Player_Info(Client).Nick;
	end;
	
	function Get_Number_Players(C4_Game: in C4_Game_Type) return Natural is
	begin
		return C4_Game.Current_Players;
	end;
	
	function Get_Max_Players (C4_Game: in C4_Game_Type) return Natural is
	begin
		return C4_Game.Max_Players;
	end;
	
	function Get_Current_Turn (C4_Game: in C4_Game_Type) return Natural is
	begin
		return C4_Game.Current_Turn;
	end;
	
	procedure Next_Turn(C4_Game: in out C4_Game_Type) is
	begin
		if C4_Game.Current_Turn = 1 then
			C4_Game.Current_Turn := 2;
		else
			C4_Game.Current_Turn := 1;
		end if;
	end Next_Turn;
	
	function Nick_Exists(C4_Game: in C4_Game_Type; Nick: ASU.Unbounded_String) return boolean is
	
		Exists: boolean := False;
		I: Integer := 1;
	begin
		while not Exists and I <= C4_Game.Current_Players loop
		
			if C4_Game.Player_Info(I).Nick /= Nick then
				I := I +1;
			else
				Exists := True;
			end if;
		
		end loop;
		if Exists then 
			return True;
		else
			return False;
		end if;
	end;
	
	function Nick_EP_Exists(C4_Game: in C4_Game_Type; Nick: ASU.Unbounded_String; EP: LLU.End_Point_Type) return boolean is
		Exists: boolean := False;
		I: Integer := 1;
	begin

		while not Exists and I <= C4_Game.Current_Players loop
		
			if C4_Game.Player_Info(I).Nick = Nick and C4_Game.Player_Info(I).EP = EP then
				Exists := True;
			else
				I := I +1;
			end if;
		
		end loop;
		if Exists then 
			return True;
		else
			return False;
		end if;
	end;

end server_game;
