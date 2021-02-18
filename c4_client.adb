with Lower_Layer_UDP;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with C4_messages;
with Client_Handler;

procedure C4_Client is

	package LLU renames Lower_Layer_UDP;
	package ATIO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package CL renames Ada.Command_Line;
	package CM renames C4_messages;
	package CH renames Client_Handler;

	use type CM.Message_Type;
	use type ASU.Unbounded_String;
	use type CH.Client_State;

	Host_Name: ASU.Unbounded_String;
	Port_Number: Integer;
	Nickname: ASU.Unbounded_String;
	Key: ASU.Unbounded_String;
	
	Expired: Boolean;

	Client_EndPoint_Receive : LLU.End_Point_Type;
	Client_EndPoint_Handler : LLU.End_Point_Type;
	Server_EndPoint : LLU.End_Point_Type;

	Buffer : aliased LLU.Buffer_Type(1024);

	Accepted: Boolean;
	Reason: ASU.Unbounded_String;
	
	Finish: Boolean;
	Fin: Boolean;

	Column: Positive;
	
	procedure Send_Message(Header_Send: CM.Message_Type) is
	begin
		LLU.Reset(Buffer);
		case Header_Send is 
			when CM.Join =>
				CM.Message_Type'Output(Buffer'access, CM.Join); 
				LLU.End_Point_Type'Output(Buffer'access, Client_EndPoint_Receive);
				LLU.End_Point_Type'Output(Buffer'access, Client_EndPoint_Handler);
				ASU.Unbounded_String'Output(Buffer'access, Nickname);
				ASU.Unbounded_String'Output(Buffer'access, Key);
		
			when CM.Logout =>
				CM.Message_Type'Output(Buffer'access, CM.Logout); 
				ASU.Unbounded_String'Output(Buffer'access, Key);
				ASU.Unbounded_String'Output(Buffer'access, Nickname);
				LLU.End_Point_Type'Output(Buffer'access, Client_EndPoint_Handler);
				
			when CM.Move =>
				CM.Message_Type'Output(Buffer'access, CM.Move);   
				ASU.Unbounded_String'Output(Buffer'access, Nickname);
				Positive'Output(Buffer'access, Column);
				ASU.Unbounded_String'Output(Buffer'access, Key);
				
			when others =>
					null;
		end case;
		LLU.Send(Server_EndPoint, Buffer'access);
	end Send_Message;
	
	procedure Confirmation_Menu is
		Finish_Game: exception;
		User_Input: ASU.Unbounded_String;
		Menu_Option: ASU.Unbounded_String;
		Response: ASU.Unbounded_String;
		
	begin
		ATIO.Put_line("Enter a column: ");
		Response := ASU.To_Unbounded_String(ATIO.Get_Line);
		
		if Response = "Q" or Response = "q" then
			ATIO.Put("Are you sure you want to leave the game? (yes/no)");
			Menu_Option := ASU.To_Unbounded_String(ATIO.Get_Line);
			if ASU.To_String(Menu_Option)  = "y" or else ASU.To_String(Menu_Option) = "yes" then
				Send_Message(CM.Logout);
				ATIO.Put_Line("You have abandoned the game.");
				CH.State := CH.FinishedGame; 
				Fin := True;
				raise Finish_Game;
				
			elsif ASU.To_String(Menu_Option)  = "n" or else ASU.To_String(Menu_Option) = "no" then
				ATIO.Put_Line("Nice! Keep playing.");
				ATIO.Put_line("Enter a column: ");
				CH.State := CH.OurTurn;
				Response := ASU.To_Unbounded_String(ATIO.Get_Line);
			end if;
		end if;
		
		Column := Integer'Value(ASU.To_String(Response));
		Send_Message(CM.Move);
		
		if CH.State /= CH.FinishedGame then
			CH.State := CH.InGame;
		end if;
		Fin := True;

	exception 
		when Constraint_Error => 
			ATIO.Put_Line("Not valid input. Choose a valid column.");	
		when Finish_Game => 
			ATIO.Put_Line("Bye! Thanks for playing.");
	end Confirmation_Menu;
	
	procedure Immediate_Quit is
		Char: Character;
		Available: Boolean;
		Menu_Option: ASU.Unbounded_String;
	begin
		ATIO.Get_Immediate(Char, Available);
		if Available then
			if Char = 'q' or Char = 'Q' then
				ATIO.Put("Are you sure you want to leave the game? (yes/no)");
				Menu_Option := ASU.To_Unbounded_String(ATIO.Get_Line);
				if ASU.To_String(Menu_Option)  = "y" or else ASU.To_String(Menu_Option) = "yes" then
					Send_Message(CM.Logout);
					ATIO.Put_Line("You have abandoned the game.");
					ATIO.Put_Line("Bye! Thanks for playing.");
					CH.State := CH.FinishedGame; 
				end if;		
			end if;
		end if;
	end Immediate_Quit;
	
	Usage_Error: exception;
	Header: CM.Message_Type;
begin
	
	if CL.Argument_Count = 3 then 
		Host_Name := ASU.To_Unbounded_String(CL.Argument(1)); 
		Port_Number := Integer'Value(CL.Argument(2));
		Nickname := ASU.To_Unbounded_String(CL.Argument(3));
		key := ASU.To_Unbounded_String(""); 
	elsif CL.Argument_Count = 4 then
		Host_Name := ASU.To_Unbounded_String(CL.Argument(1)); 
		Port_Number := Integer'Value(CL.Argument(2));
		Nickname := ASU.To_Unbounded_String(CL.Argument(3));
		Key := ASU.To_Unbounded_String(CL.Argument(4));
	else
		raise Usage_Error;
	end if;
	
	Server_EndPoint := LLU.Build(LLU.To_IP(ASU.To_String(Host_Name)), Port_Number);
	LLU.Bind_Any(Client_EndPoint_Receive);
	LLU.Bind_Any(Client_EndPoint_Handler, Client_Handler.Client_Handler'access);
	
	Send_Message(CM.Join);
	
	LLU.Reset(Buffer);
	LLU.Receive(Client_EndPoint_Receive, Buffer'Access, 10.0, Expired);
	
	if Expired then
		ATIO.Put_Line("Could not join. Unreachable server.");
	else
		Header := CM.Message_Type'Input(Buffer'access);
		
		if Header = CM.Welcome then
			Accepted := Boolean'Input(Buffer'access);
			Reason := ASU.Unbounded_String'Input(Buffer'access);
			Key := ASU.Unbounded_String'Input(Buffer'access);
			
			if Accepted then
				ATIO.Put("C4 Game Client:  Welcome " & ASU.To_String(Nickname));
				ATIO.Put_Line(" - Game key: " & ASU.To_String(Key));
				ATIO.Put_Line("Waiting for the game to start...");
					
				loop
					case CH.State is
						when CH.WaitingForGame =>
							Immediate_Quit;
							
						when CH.InGame =>
							Immediate_Quit;

						when CH.OurTurn =>
							Fin := False;
							loop
								begin
									Confirmation_Menu;
								end;
							exit when Fin;	
							end loop;
							
						when CH.MoveRejected =>
							Fin := False;
							loop
								ATIO.Put_line("Rejected move.");
								begin
									Confirmation_Menu;			
								end;
							exit when Fin;	
							end loop;
							
						when CH.FinishedGame =>
							Finish := True;
							
						when others =>
							null;
						end case;
				exit when Finish;
				end loop;
				
			else
				ATIO.Put_Line(ASU.To_String(Reason)); 
			end if;
		else
			ATIO.Put_Line("Invalid message received.");
		end if;
	end if;	
			
	LLU.Finalize;		
exception
	when Usage_Error => ATIO.Put_Line("Invalid Arguments."); 
	LLU.Finalize;										
	
end C4_Client;
