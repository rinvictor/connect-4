with Lower_Layer_UDP;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with c4_messages;
with server_game;
with Vertical_Dashboard;
with Server_Handler;

procedure c4_server is

	package LLU renames Lower_Layer_UDP;
	package ATIO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package CL renames Ada.Command_Line;
	package CM renames c4_messages;
	package VD renames Vertical_Dashboard;
	use type CM.Message_Type;
	package SG renames server_game;
	package SH renames Server_Handler;
	
	use type ASU.Unbounded_String;
	
	Usage_Error: exception;
	Column_full: exception;
	
	NumArguments: constant integer := 1;
	NumMaxPlayers: constant integer := 2;
	
	Port_Number: Integer;
	
	Server_EndPoint: LLU.End_Point_Type;
	
	Nickname: ASU.Unbounded_String;
	C4_Game: SG.C4_Game_Type;

	Dashboard: ASU.Unbounded_String;
	
	User_Nick: ASU.Unbounded_String;
		
	Available: Boolean;
	Char: Character;
	
begin
	
	if CL.Argument_Count /= NumArguments then
		raise Usage_Error;
	end if;

	Port_Number := Integer'Value(CL.Argument(1));
	Server_EndPoint := LLU.Build(LLU.To_IP(LLU.Get_Host_Name), Port_Number);
	LLU.Bind(Server_EndPoint, Server_Handler.Server_Handler'access); 
	
	ATIO.Put_line("Listening on port" & Integer'Image(Port_Number) & "...");
	
	loop
		ATIO.Get_Immediate(Char, Available);
		if Available then 
			if Char = 'N' or Char = 'n' then
				ATIO.Put_line("===== Current number of games =====");
				if SH.ABB_Maps.Map_Length(SH.Binary_Tree) = 1 then
					ATIO.Put_Line("Server is hosting " & Natural'Image(SH.ABB_Maps.Map_Length(SH.Binary_Tree)) & " game.");
				else
					ATIO.Put_Line("Server is hosting " & Natural'Image(SH.ABB_Maps.Map_Length(SH.Binary_Tree)) & " games.");
				end if;
				ATIO.Put_line("===================================");
				ATIO.New_Line;
			else
				ATIO.Put_Line("Press 'N' to get the number of games.");
			end if;
		end if;
	end loop;
exception
	when Usage_Error => 
	ATIO.Put_Line("Invalid Arguments");
	LLU.Finalize;

end C4_Server;

