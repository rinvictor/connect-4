with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with C4_Messages;
with Lower_Layer_UDP;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with c4_messages;
with server_game;
with Vertical_Dashboard;
with Ada.Command_Line;
with Abb_maps_G;

package Server_Handler is

	package LLU renames Lower_Layer_UDP;
	package ATIO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	use type ASU.Unbounded_String;
	package CL renames Ada.Command_Line;
	package CM renames c4_messages;
	package VD renames Vertical_Dashboard;
	use type CM.Message_Type;
	package SG renames server_game;

	package ABB_Maps is new ABB_Maps_G(
		Key_Type => ASU.Unbounded_String, 
		Value_Type => SG.C4_Game_Type, 
		"=" => ASU."=", 
		"<" => ASU."<", 
		">" => ASU.">", 
		Key_To_String => ASU.To_String); 

	Binary_Tree: ABB_Maps.Map;
	
	procedure Server_Handler(From: in LLU.End_Point_Type;
								To: in LLU.End_Point_Type;
								P_Buffer: access LLU.Buffer_Type);
								
end Server_Handler;
