with abb_maps_g;

package body Server_Handler is 

	MinColumn: constant Integer := 1;
	MaxColumn: constant Integer := 10;
	NumMaxPlayers: constant Integer := 2;
	

	procedure Send_Welcome_Message(Welcome_case: String; Game_Key: ASU.Unbounded_String; Nickname: ASU.Unbounded_String; Client_EndPoint: LLU.End_Point_Type) is
		Buffer : aliased LLU.Buffer_Type(1024);
	begin
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'access, CM.Welcome); 
		if Welcome_Case = "Game_Is_Full_Case" then
			Boolean'Output(Buffer'access, False);  
			ASU.Unbounded_String'Output(Buffer'access, ASU.To_Unbounded_String("Could not join, game is full.")); 
			ATIO.Put_Line("Key: " & ASU.To_String(Game_Key) & " - " & ASU.To_String(Nickname) & " could not join, the game is already full.");
			
		elsif Welcome_Case= "Username_In_Use_Case" then
			Boolean'Output(Buffer'access, False);
			ASU.Unbounded_String'Output(Buffer'access, ASU.To_Unbounded_String("Could not join. Username already in use."));
			ATIO.Put_Line(ASU.To_String(Nickname) & " could not join, username is already in use.");		
		
		elsif Welcome_Case = "Welcome_For_Empty_Key_Case" then
			Boolean'Output(Buffer'access, True);
			ASU.Unbounded_String'Output(Buffer'access, Nickname);
			ASU.Unbounded_String'Output(Buffer'access, Nickname);
			ATIO.Put_Line("Key: " & ASU.To_String(Nickname) & " - " & ASU.To_String(Nickname) & " joined successfully.");
		else	
			Boolean'Output(Buffer'access, True);
			ASU.Unbounded_String'Output(Buffer'access,ASU.To_Unbounded_String(""));
			
		end if;
		ASU.Unbounded_String'Output(Buffer'access, Game_Key); 
		LLU.Send(Client_EndPoint, Buffer'access); 
	end Send_Welcome_Message;
	
						
	procedure Send_Server_Message(Server_Case: String; c4_game: SG.C4_Game_Type) is
		Buffer : aliased LLU.Buffer_Type(1024);
	begin
		LLU.Reset(Buffer); 
		if Server_Case = "Send_Info" then
			for I in 1..SG.Get_Number_Players(c4_game) - 1 loop 
				CM.Message_Type'Output(Buffer'access, CM.Server);
				ASU.Unbounded_String'Output(Buffer'access, ASU.To_Unbounded_String("New player joins the game" 
												& Integer'Image(SG.Get_Number_Players(c4_game)) & " /" 
												& Integer'Image(NumMaxPlayers) & " players"));
				LLU.Send(SG.Get_CLient_EP(C4_Game, I), Buffer'access); 
			end loop;													
		else 
				CM.Message_Type'Output(Buffer'access, CM.Server);
				ASU.Unbounded_String'Output(Buffer'access, VD.Dashboard_To_US(SG.Get_Dashboard(C4_Game).all));  
				LLU.Send(SG.Get_CLient_EP(C4_Game, SG.Get_Current_Turn(C4_Game)), Buffer'access); 
		end if;
	end Send_Server_Message;
	
	
	procedure Send_StartGame_Message(c4_game: SG.C4_Game_Type) is 
		Buffer : aliased LLU.Buffer_Type(1024);
	begin
		for I in 1..SG.Get_Number_Players(c4_game) loop
			LLU.Reset(Buffer);
			CM.Message_Type'Output(Buffer'access, CM.StartGame);  
			LLU.Send(SG.Get_CLient_EP(C4_Game, I), Buffer'access);
		end loop;
	end Send_StartGame_Message;
	
	
	procedure Send_YourTurn_Message(c4_game: SG.C4_Game_Type) is
		Buffer : aliased LLU.Buffer_Type(1024);
	begin
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'access, CM.YourTurn);
		LLU.Send(SG.Get_CLient_EP(C4_Game, SG.Get_Current_Turn(C4_Game)), Buffer'access); 
	end Send_YourTurn_Message;
	

	procedure When_Player_Accepted(c4_game: SG.C4_Game_Type; Game_Key: ASU.Unbounded_String; 
									Nickname: ASU.Unbounded_String; Client_EndPoint: LLU.End_Point_Type; Print_key: Boolean) is	
	begin
		Send_Welcome_Message("Player_accepted", Game_Key, Nickname, Client_EndPoint); 
		
		if Print_key then
			ATIO.Put_Line("Key: " & ASU.To_String(Game_Key) & " - " & ASU.To_String(Nickname) & " joined successfully.");
		end if;
			
		Send_Server_Message("Send_Info", C4_Game); 
		if SG.Get_Number_Players(c4_game) = NumMaxPlayers then
			ATIO.Put_Line("--GAME " & ASU.To_String(Game_Key) & " STARTED--");
			Send_StartGame_Message(C4_Game);
			Send_Server_Message("Send_Dash", C4_Game); 
			Send_YourTurn_Message(C4_Game);  
		end if;
	

	end When_Player_Accepted;
	 
	
	procedure Send_EndGame_Message(EndGame_Case: String; c4_game: SG.C4_Game_Type; Nickname: ASU.Unbounded_String) is
		Buffer : aliased LLU.Buffer_Type(1024);
	begin
		for I in 1..SG.Get_Number_Players(c4_game) loop 
		LLU.Reset(Buffer); 
		CM.Message_Type'Output(Buffer'access, CM.EndGame);
		
		if EndGame_Case = "Winner" then 
			ASU.Unbounded_String'Output(Buffer'access, Nickname); 
			ASU.Unbounded_String'Output(Buffer'access, VD.Dashboard_To_US(SG.Get_Dashboard(C4_Game).all)); 
			ASU.Unbounded_String'Output(Buffer'access, ASU.To_Unbounded_String("")); 
			if SG.Get_CLient_Name(C4_Game, I) = Nickname then
				Boolean'Output(Buffer'access, True);
			else
				Boolean'Output(Buffer'access, False);
			end if;
			
		elsif EndGame_Case = "Dashboard_Full" then
			ASU.Unbounded_String'Output(Buffer'access, ASU.To_Unbounded_String(""));
			ASU.Unbounded_String'Output(Buffer'access, VD.Dashboard_To_US(SG.Get_Dashboard(C4_Game).all));
			ASU.Unbounded_String'Output(Buffer'access, ASU.To_Unbounded_String(""));
			Boolean'Output(Buffer'access, False);
		else
			ASU.Unbounded_String'Output(Buffer'access, ASU.To_Unbounded_String("")); 
			ASU.Unbounded_String'Output(Buffer'access,	ASU.To_Unbounded_String("") );
			ASU.Unbounded_String'Output(Buffer'access, Nickname);
			Boolean'Output(Buffer'access, False);
		end if;
		LLU.Send(SG.Get_CLient_EP(C4_Game, I), Buffer'access);
		
		end loop;
	end Send_EndGame_Message;
	
	
	procedure Send_MoveReceived_Message(Correct: boolean; c4_game: SG.C4_Game_Type) is
		Buffer : aliased LLU.Buffer_Type(1024);
	begin
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'access, CM.MoveReceived);
		if not Correct then
			Boolean'Output(Buffer'access, False);
		else 
			Boolean'Output(Buffer'access, True);
		end if;
		LLU.Send(SG.Get_CLient_EP(C4_Game, SG.Get_Current_Turn(C4_Game)), Buffer'access);
	end Send_MoveReceived_Message;


	function Client_Nick_Already_Exists(ABB: in Abb_Maps.Map; 
										Client_Nick: in ASU.Unbounded_String) return Boolean is
		C4_Game: SG.C4_Game_Type;
		Left, Right: Abb_Maps.Map;
	begin
		if abb_maps.Is_Empty(ABB) then													
			return False;
		else
			C4_Game := Abb_Maps.Get_Value(ABB);
			Left := Abb_Maps.Get_Left(ABB);
			Right := Abb_Maps.Get_Right(ABB);
			
			return 
				SG.Nick_Exists(C4_Game, Client_Nick) or else 
				Client_Nick_Already_Exists(Left, Client_Nick) or else
				Client_Nick_Already_Exists(Right, Client_Nick);
		end if;
	end Client_Nick_Already_Exists;
	

	procedure Empty_Place_For_Player(ABB: in Abb_Maps.Map; 
									Game_Key: out ASU.Unbounded_String;
									C4_Game: out SG.C4_Game_Type;
									Found: out Boolean) is
		Game_Aux: SG.C4_Game_Type;
		Left, Right: Abb_Maps.Map;
	begin
		if Abb_Maps.Is_Empty(ABB) then
			Found := False;
		else
			Game_Key := Abb_Maps.Get_Key(ABB);
			Game_Aux := Abb_Maps.Get_Value(ABB);
			Left := Abb_Maps.Get_Left(ABB);
			Right := Abb_Maps.Get_Right(ABB);
			
			if SG.Get_Number_Players(Game_Aux) /= SG.Get_Max_Players(Game_Aux) then
				Found := True;
				C4_Game := Game_Aux;
			else
				Empty_Place_For_Player(Left, Game_Key, C4_Game, Found);
				if not Found then
					Empty_Place_For_Player(Right, Game_Key, C4_Game, Found);
				end if;
			end if;
		end if;
	end Empty_Place_For_Player;
	

	function Is_Valid_Column (Column: Positive) return Boolean is
	begin
		if Column >= MinColumn and Column <= MaxColumn then
			return True;
		else
			return False;
		end if;
	end Is_Valid_Column;


	procedure Join_Handler(P_Buffer: access LLU.Buffer_Type) is
		C4_Game: SG.C4_Game_Type;
		Success: Boolean;
		Found: Boolean;
		Game_Key: ASU.Unbounded_String;
		Nickname: ASU.Unbounded_String;
		Client_EndPoint: LLU.End_Point_Type;
		Client_EndPoint_Handler: LLU.End_Point_Type;
	begin
		Client_EndPoint := LLU.End_Point_Type'Input(P_Buffer) ;
		Client_EndPoint_Handler := LLU.End_Point_Type'Input(P_Buffer) ;
		Nickname := ASU.Unbounded_String'Input(P_Buffer);
		Game_Key := ASU.Unbounded_String'Input(P_Buffer);
		
		if Client_Nick_Already_Exists(Binary_Tree, Nickname) then  
			Send_Welcome_Message("Username_In_Use_Case", Game_Key, Nickname, Client_EndPoint); 
			
		elsif Game_key /= "" then
				
				ABB_Maps.Get(Binary_Tree,Game_Key, C4_Game, Success);
				if Success then 
					if SG.Get_Number_Players(C4_Game) = NumMaxPlayers then 
						Send_Welcome_Message("Game_Is_Full_Case", Game_Key, Nickname, Client_EndPoint);
					else 
						SG.Set_Player_Info(C4_Game, Nickname, Client_EndPoint_Handler); 
						ABB_Maps.Put(Binary_Tree, Game_Key, C4_Game); 
						When_Player_Accepted(C4_Game, Game_Key, Nickname, Client_EndPoint, True);
					end if;
				else 
					SG.Set_Player_Info(C4_Game, Nickname, Client_EndPoint_Handler);
					ABB_Maps.Put(Binary_Tree, Game_Key, C4_Game);
					When_Player_Accepted(C4_Game, Game_Key, Nickname, Client_EndPoint, True);
				end if;
					
		else
			Empty_Place_For_Player(Binary_Tree, Game_key, C4_Game, Found);
			
			if Found then 
				SG.Set_Player_Info(C4_Game, Nickname, Client_EndPoint_Handler);
				ABB_Maps.Put(Binary_Tree,Game_Key, C4_Game);
				When_Player_Accepted(C4_Game, Game_Key, Nickname, Client_EndPoint, True);
			else 
				SG.Set_Player_Info(C4_Game, Nickname, Client_EndPoint_Handler);
				ABB_Maps.Put(Binary_Tree, Nickname, C4_Game);
				Send_Welcome_Message("Welcome_For_Empty_Key_Case", Game_Key, Nickname, Client_EndPoint);
				When_Player_Accepted(C4_Game, Game_Key, Nickname, Client_EndPoint, False);
			end if;			
		end if;

	end Join_Handler;

	procedure Move_Handler(P_Buffer: access LLU.Buffer_Type) is 
		C4_Game: SG.C4_Game_Type;
		Success: Boolean;
		Column: Integer;
		Winner: Boolean;
		Column_full: exception;
		Game_Key: ASU.Unbounded_String;
		Nickname: ASU.Unbounded_String;

	begin
		Nickname := ASU.Unbounded_String'Input(P_Buffer);
		Column := Integer'Input(P_Buffer);
		Game_Key := ASU.Unbounded_String'Input(P_Buffer);
		ABB_Maps.Get(Binary_Tree, Game_Key, C4_Game, Success);
		
		
		if Success then 
			if Is_Valid_Column(Column) then
			
				if VD.Is_Column_Full(SG.Get_Dashboard(c4_game).all, Column) then
					raise Column_full; 
				end if;
				
				Send_MoveReceived_Message(True, C4_Game);
				ATIO.Put_Line("Key: " & ASU.To_String(Game_Key) & " - " & ASU.To_String(Nickname) & "'s move: " & Positive'Image(Column));
				
				VD.Put_Token(SG.Get_Dashboard(c4_game).all, Column, SG.Get_Current_Turn(C4_Game), Winner); 	

				if Winner then 
					
					Send_EndGame_Message("Winner", C4_Game, Nickname);
					ATIO.Put_Line("Key: " & ASU.To_String(Game_Key) & " - " & ASU.To_String(Nickname) & " has won the game!");
					ATIO.Put_Line(ASU.To_String(VD.Dashboard_To_US(SG.Get_Dashboard(C4_Game).all)));
					ABB_Maps.Delete(Binary_Tree,Game_Key, Success);

				elsif VD.Dashboard_Is_Full((SG.Get_Dashboard(C4_Game).all)) then 
				
					Send_EndGame_Message("Dashboard_Full", C4_Game, Nickname);		
					ATIO.Put_Line("Key: " & ASU.To_String(Game_Key) & " - " & "Dashboard is full. End of the game");
					ATIO.Put_Line(ASU.To_String(VD.Dashboard_To_US(SG.Get_Dashboard(C4_Game).all)));			
					ABB_Maps.Delete(Binary_Tree,Game_Key, Success);
				else 
					SG.Next_Turn(C4_Game);	
					ABB_Maps.Put(Binary_Tree,Game_Key, C4_Game); 
					Send_Server_Message("Send_Dash", C4_Game);
					Send_YourTurn_Message(C4_Game);
					
				end if;
				
			else 
				Send_MoveReceived_Message(False, C4_Game);
				ATIO.Put_line("Key: " & ASU.To_String(Game_Key) & " - " & "Not valid column.");
			end if;
		else
			ATIO.Put_Line("Unexpected message received");
		end if;
	exception
		when Column_full =>
			Send_MoveReceived_Message(False, C4_Game);
			ATIO.Put_Line("Key: " & ASU.To_String(Game_Key) & " - " & "Column is full.");
			ATIO.Put_Line(ASU.To_String(VD.Dashboard_To_US(SG.Get_Dashboard(C4_Game).all)));
	end Move_Handler;

	procedure Logout_Handler(P_Buffer: access LLU.Buffer_Type) is 
		C4_Game: SG.C4_Game_Type; 
		Success: Boolean;
		Game_Key: ASU.Unbounded_String;
		Nickname: ASU.Unbounded_String;
		Client_EndPoint_Handler: LLU.End_Point_Type;
	begin
		Game_Key := ASU.Unbounded_String'Input(P_Buffer);
		Nickname := ASU.Unbounded_String'Input(P_Buffer);  
		Client_EndPoint_Handler := LLU.End_Point_Type'Input(P_Buffer);
		
		ABB_Maps.Get(Binary_Tree, Game_Key, C4_Game, Success);
		if Success then
			if SG.Nick_EP_Exists(C4_Game, Nickname, Client_EndPoint_Handler) then 
				Send_EndGame_Message("End", C4_Game, Nickname);
				ATIO.Put_line("Key: " & ASU.To_String(Game_Key) & " - " & "Player " & ASU.To_String(Nickname) & " has abandoned the game.");
				ATIO.Put_line("Key: " & ASU.To_String(Game_Key) & " - " & "End of the game");
				ABB_Maps.Delete(Binary_Tree,Game_Key, Success);
			else
				ATIO.Put_Line("Key: " & ASU.To_String(Game_Key) & " - " & "Player already exists");
			end if;
		else
			ATIO.Put_line("Error: Game not found");
		end if;
	end Logout_Handler;


	procedure Server_Handler(From: in LLU.End_Point_Type;
								To: in LLU.End_Point_Type;
								P_Buffer: access LLU.Buffer_Type) is
		Header: CM.Message_Type; 
	begin		
		Header := CM.Message_Type'Input(P_Buffer);
		case Header is 
			when CM.Join =>
				Join_Handler(P_Buffer); 
			when CM.Move =>
				Move_Handler(P_Buffer);
			when CM.Logout =>
				Logout_Handler(P_Buffer);
			when others =>
				null;
		end case;
		LLU.Reset(P_Buffer.all);
	end Server_Handler;

end Server_Handler;
