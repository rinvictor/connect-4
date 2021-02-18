package body Vertical_Dashboard is

	package ATIO renames Ada.Text_IO;
	use type ASU.Unbounded_String;

	function Has_4_Tokens(N_Tokens: integer) return boolean is
		N_Tokens_4: Boolean := False;
	begin
		if N_Tokens >= 4 then
			N_Tokens_4 := True;
		else
			N_Tokens_4 := False;
		end if;
		
		return N_Tokens_4;
	end Has_4_Tokens;
	
	
	function Horizontal_Win(Dashboard: in Board_Type; Row: in Integer; 
			Column: in Integer; Player: in Integer) return boolean is 
		N_Tokens: integer := 0;
		Column_Aux: integer := Dashboard'First(2);
		Winner: Boolean := False;
	begin
		while N_Tokens /= 4 and Column_Aux <= Dashboard'Last(2) loop
			if Dashboard(Row, Column_Aux).Empty = False and Dashboard(Row, Column_Aux).Player = Player then
				N_Tokens := N_Tokens + 1;
			else
				N_Tokens := 0;
			end if;
			Column_Aux := Column_Aux + 1;
		end loop;

		Winner := Has_4_Tokens(N_Tokens);
		return Winner;
	end Horizontal_Win;
	
	function Vertical_Win(Dashboard: in Board_Type; Row: in Integer; Column: in Integer; Player: in Integer) return boolean is 
		Row_Aux: integer := Dashboard'First(1);
		N_Tokens: Integer := 0;
		Winner: Boolean := False;
	begin

		while N_Tokens /= 4 and Row_Aux >= Dashboard'First(1) and Row_Aux <= Dashboard'Last(1) loop
			if Dashboard(Row_Aux, Column).Player = Player then
				N_Tokens := N_Tokens + 1;
			else
				N_Tokens := 0;
			end if;
			Row_Aux := Row_Aux + 1;
		end loop;
		
		Winner := Has_4_Tokens(N_Tokens);
		return Winner;
	end Vertical_Win;


	function Diagonal_LefttoRight_Win(Dashboard: in Board_Type; Row: in Integer; 
			Column: in Integer; Player: in Integer) return boolean is 
		N_Tokens: Natural;
		Row_Aux: Integer;
		Column_Aux: Integer;
		Winner: Boolean;
	begin
		N_Tokens := 0;
		Row_Aux := Row;
		Column_Aux := Column;
		Winner := False;
		while Row_Aux < Dashboard'Last(1) and Column_Aux > Dashboard'First(2) loop
			Row_Aux := Row_Aux + 1;
			Column_Aux := Column_Aux - 1;
		end loop;

		while N_Tokens /= 4 and Row_Aux >= Dashboard'First(1) and Column_Aux <= Dashboard'Last(2) loop
			if not Dashboard(Row_Aux, Column_Aux).Empty and Dashboard(Row_Aux, Column_Aux).Player = Player then
				N_Tokens := N_Tokens + 1;
			else
				N_Tokens := 0;
			end if;
			
			Row_Aux := Row_Aux - 1;
			Column_Aux := Column_Aux + 1;
			
		end loop;
		Winner := Has_4_Tokens(N_Tokens);
		
		return Winner;
		
	end Diagonal_LefttoRight_Win;
		
	function Diagonal_RigthtoLeft_Win(Dashboard: in Board_Type; Row: in Integer; 
			Column: in Integer; Player: in Integer) return boolean is
		N_Tokens: Natural;
		Row_Aux: Integer;
		Column_Aux: Integer;
		Winner:Boolean;
	begin
		N_Tokens := 0;
		Row_Aux := Row;
		Column_Aux := Column;
		Winner := False;
		while Row_Aux < Dashboard'Last(1) and Column_Aux < Dashboard'Last(2) loop
			Row_Aux := Row_Aux + 1;
			Column_Aux := Column_Aux + 1;
		end loop;
		
		while N_Tokens /= 4 and Row_Aux >= Dashboard'First(1) and Column_Aux >= Dashboard'First(2) loop
			if not Dashboard(Row_Aux, Column_Aux).Empty and Dashboard(Row_Aux, Column_Aux).Player = Player then
				N_Tokens := N_Tokens + 1;
			else
				N_Tokens := 0;
			end if;
			
			Row_Aux := Row_Aux - 1;
			Column_Aux := Column_Aux - 1;
			
		end loop;
		
		Winner := Has_4_Tokens(N_Tokens);
		
		return Winner;
	end Diagonal_RigthtoLeft_Win;
	
	
	function Player_Wins(Dashboard: in Board_Type; Row: in Integer; 
		Column: in Integer; Player: in Integer) return Boolean is
	
	begin
		return (Vertical_Win(Dashboard, Row, Column, Player)) or else 
			(Horizontal_Win(Dashboard, Row, Column, Player)) or else 
			(Diagonal_RigthtoLeft_Win(Dashboard, Row, Column, Player)) or else
			(Diagonal_LefttoRight_Win(Dashboard, Row, Column, Player));
	end Player_Wins;
	
	
	procedure Put_Token(Dashboard: in out Board_Type; Column: in Integer; Player: in Integer; Winner: out Boolean) is
		Row: Integer;
		Filled: Boolean;
	
	begin
		Filled := False;
		Row := Dashboard'Last(1);
			while not Filled and Row >= Dashboard'First(1) loop
				if Dashboard(Row, Column).Empty then
					Dashboard(Row, Column).Player := Player;
					Dashboard(Row, Column).Empty := False;
					Filled := True;
					Winner := Player_Wins(Dashboard, Row, Column, Player);
				else
					Row := Row - 1;
				end if;
			end loop;
	
		end Put_Token;


	function Dashboard_Is_Full(Dashboard: in Board_Type) return Boolean is
		Column_Aux: Integer := Dashboard'First(2);
		Is_Full: Boolean := True;
		Row: Integer := Dashboard'First(1);
	begin
	while Column_Aux <= Dashboard'Last(2) and Is_Full = True loop
		if Dashboard(Row, Column_Aux).Empty = False then
			Is_Full := True;
		else
			Is_Full := False;
		end if;
	Column_Aux := Column_Aux + 1;
	end loop;
	
	return Is_Full;
	end Dashboard_Is_Full;


	function Dashboard_To_US(Dashboard: Board_Type) return ASU.Unbounded_String is
		US_Dashboard: ASU.Unbounded_String;
	begin
		for Row in Dashboard'Range(1) loop
			for Column in Dashboard'Range(2) loop
				US_Dashboard := US_Dashboard & ASU.To_Unbounded_String(" ");
				if Dashboard(Row, Column).Empty then
					US_Dashboard := US_Dashboard & ASU.To_Unbounded_String("-"); 
				else
					if Dashboard(Row, Column).Player = 1 then
						US_Dashboard := US_Dashboard & ASU.To_Unbounded_String(Ada.Characters.Latin_1.ESC & "[91m" & 
							"O" & Ada.Characters.Latin_1.ESC & "[0m");
					else
						US_Dashboard := US_Dashboard & ASU.To_Unbounded_String(Ada.Characters.Latin_1.ESC & "[93m" & 
							"X" & Ada.Characters.Latin_1.ESC & "[0m");
					end if;	
				end if;
			end loop;
			US_Dashboard := US_Dashboard & ASCII.LF;
		end loop;
		return US_Dashboard;
	end Dashboard_To_US;

	
	function Is_Column_Full(Dashboard: Board_Type; Column: Integer) return boolean is
		full: boolean := False;
	begin
		if Dashboard(Dashboard'First(1), Column).Empty = True then
			return full = True;
		else
			return full = False;
		end if;
			
	end Is_Column_Full;

end vertical_dashboard;
