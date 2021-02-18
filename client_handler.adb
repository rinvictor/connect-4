package body Client_Handler is

	package CM renames C4_Messages;
	package ATIO renames Ada.Text_IO;
	
	use type ASU.Unbounded_String;
	
	
	procedure Client_Handler (From: in LLU.End_Point_Type; 
		To: in LLU.End_Point_Type; 
		Buffer: access LLU.Buffer_Type) is
		
		Header: CM.Message_Type;
		Message: ASU.Unbounded_String;
		Accepted: Boolean;
		Winner: ASU.Unbounded_String;
		Dashboard: ASU.Unbounded_String;
		Quitter: ASU.Unbounded_String;
		YouWin: Boolean;

	begin
		Header := CM.Message_Type'Input(Buffer);
			
		case Header is
			when CM.StartGame =>
				State := InGame;
				ATIO.Put_Line("--Game started--");
 				
			when CM.Server =>
				Message := ASU.Unbounded_String'Input(Buffer);
				ATIO.Put_Line(ASU.To_String(Message));
				
			when CM.YourTurn =>
				State:= OurTurn;
				ATIO.Put_Line("This is your turn.");
				
			when CM.MoveReceived =>
				Accepted := Boolean'Input(Buffer);
				if Accepted then
					State := InGame;
				else
					State := MoveRejected;
				end if;
				
			when CM.EndGame =>
				Winner := ASU.Unbounded_String'Input(Buffer);
				Dashboard := ASU.Unbounded_String'Input(Buffer);
				Quitter := ASU.Unbounded_String'Input(Buffer);
				YouWin := Boolean'Input(Buffer);
												
				if Winner = "" then 
					if Quitter = "" then
						ATIO.Put_Line("Dashboard is full. End of the game.");
						ATIO.Put_Line(ASU.To_String(Dashboard));
					else
						if Quitter = Nickname then
							null;
						else
							ATIO.Put_Line("Player " & ASU.To_String(Quitter) & " left the game successfully.");
						end if;
					end if;
					
				else 
					if YouWin then
						ATIO.Put_Line(ASU.To_String(Winner) & ", you have won the game, congrats!");
					else
						ATIO.Put_Line(ASU.To_String(Winner) & " has won the game! Thanks for playing.");							
					end if;
						ATIO.Put_Line(ASU.To_String(Dashboard));
				end if;
				
				State := FinishedGame;
			when others =>
				null;
		end case;
	end Client_Handler;


end Client_Handler;
