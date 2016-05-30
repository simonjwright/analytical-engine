with Ada.Text_IO; use Ada.Text_IO;
package body Analytical_Engine.Annunciator_Panel.Command_Line is

   procedure Log_Attendant_Message (Panel : Instance; Msg : String)
   is
      pragma Unreferenced (Panel);
   begin
      Put_Line (Msg);
   end Log_Attendant_Message;

   procedure Log_Trace_Message (Panel : Instance; Msg : String)
   is
      pragma Unreferenced (Panel);
   begin
      Put_Line (Msg);
   end Log_Trace_Message;

   procedure Initialize (This : in out Instance)
   is
      pragma Unreferenced (This);
   begin
      Put_Line ("annunciator_panel.command_line initialized");
   end Initialize;

end Analytical_Engine.Annunciator_Panel.Command_Line;
