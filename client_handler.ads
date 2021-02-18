with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with C4_Messages;
with Ada.Text_IO;

package Client_Handler is
	package ASU renames Ada.Strings.Unbounded;
	package LLU renames Lower_Layer_UDP;

	
	type Client_State is (WaitingForGame, InGame, OurTurn, MoveRejected, FinishedGame);
	
	State: Client_State := WaitingForGame;
	
	Nickname: ASU.Unbounded_String;
	
	procedure Client_Handler(From: in LLU.End_Point_Type;
								To: in LLU.End_Point_Type;
								Buffer: access LLU.Buffer_Type);

end Client_Handler;
