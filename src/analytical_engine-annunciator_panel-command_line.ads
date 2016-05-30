package Analytical_Engine.Annunciator_Panel.Command_Line is

   type Instance is new Annunciator_Panel.Instance with private;

   overriding
   procedure Log_Attendant_Message (Panel : Instance; Msg : String);

   overriding
   procedure Log_Trace_Message (Panel : Instance; Msg : String);

private

   type Instance is new Annunciator_Panel.Instance with null record;

   overriding
   procedure Initialize (This : in out Instance);

end Analytical_Engine.Annunciator_Panel.Command_Line;
