package body Analytical_Engine.Annunciator_Panel is

   procedure Set_Tracing (This : in out Instance; To : Boolean) is
   begin
      This.Tracing := To;
   end Set_Tracing;

end Analytical_Engine.Annunciator_Panel;
